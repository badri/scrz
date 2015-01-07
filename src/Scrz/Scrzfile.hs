{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Scrz.Scrzfile
    ( Context(..)
    , mkContext
    , Scrzfile(..)
    , BuildInstruction(..)
    , Cmd(..)
    , Binding(..)
    , parseScrzfile
    ) where


import Control.Applicative
import Data.AppContainer.Types
import Data.Attoparsec.Text
import Data.Monoid
import Data.Char
import Data.Maybe
import Scrz.Types
import qualified Data.Map as M
import System.Directory (getCurrentDirectory)


import           Data.Text (Text)
import qualified Data.Text as T

import Debug.Trace


data Context = Context
    { ctxRoot :: !Text
    , ctxImage :: !Text
    } deriving (Show)

mkContext :: Text -> Scrz Context
mkContext image = Context
    <$> scrzIO (fmap T.pack getCurrentDirectory)
    <*> pure image

render :: Context -> Text -> Text
render Context{..} input = foldl f input
    [ ( "$root",  ctxRoot )
    , ( "$image", ctxImage )
    ]

  where
    f prev (needle, replacement) = T.replace
        needle replacement prev


data Scrzfile = Scrzfile
    { name :: !Text
    , baseImage :: !Text
    , buildInstructions :: ![BuildInstruction]
    , app :: !(Maybe App)
    } deriving (Show)

data BuildInstruction
    = Run !Cmd
    | Spawn ![Binding] !Cmd
    deriving (Show)

data Cmd = Cmd !Text ![Text]
    deriving (Show)

data Binding = Binding !Text !Text
    deriving (Show)

data Section
    = Name !Text
    | BaseImage !Text
    | BuildInstructions ![BuildInstruction]
    | AppS [AppDef]
    deriving (Show)

data AppDef
    = Exec !Text !Text ![Text]
    | Environment ![(Text, Text)]
    | MountPoints ![(Text, Text, Bool)]
    | Ports ![(Text, Text, Int, Bool)]
    deriving (Show)


parseScrzfile :: Context -> Text -> Either String Scrzfile
parseScrzfile ctx input = parseOnly (scrzfileParser ctx) input


scrzfileParser :: Context -> Parser Scrzfile
scrzfileParser ctx = do
    sections <- many' $ do
        skipSpace
        sec <- sectionParser ctx
        skipSpace
        return sec

    -- trace (show sections) $ return ()

    Scrzfile
        <$> findSection "name" sections nameS
        <*> findSection "base image" sections baseImageS
        <*> findSection "build instructions" sections buildInstructionsS
        <*> findSection "app" sections appDefS

  where
    findSection n [] _ = fail $ "Section not found: " <> n
    findSection n (x:xs) f = case f x of
        Nothing -> findSection n xs f
        Just x -> pure x

    nameS (Name n) = Just n
    nameS _        = Nothing

    baseImageS (BaseImage n) = Just n
    baseImageS _        = Nothing

    buildInstructionsS (BuildInstructions n) = Just n
    buildInstructionsS _        = Nothing

    appDefS (AppS ad) = fmap Just $ App
        <$> fromMaybeM (fail "exec not found") (findAppDef "exec" ad execS)
        <*> fromMaybeM (fail "uid not found")  (findAppDef "uid" ad uidS)
        <*> fromMaybeM (fail "gid not found")  (findAppDef "gid" ad gidS)
        <*> pure [] -- event handlers
        <*> fromMaybeM (pure M.empty) (findAppDef "env" ad envS)
        <*> fromMaybeM (pure []) (findAppDef "mount points" ad mpS)
        <*> fromMaybeM (pure []) (findAppDef "ports" ad portsS)

    appDefS _        = Nothing


    fromMaybeM d mbR = do
        maybe d pure mbR


    findAppDef n [] _ = Nothing
    findAppDef n (x:xs) f = case f x of
        Nothing -> findAppDef n xs f
        Just x -> Just x

    execS (Exec _ _ exec) = Just exec
    execS _ = Nothing

    uidS (Exec uid _ _) = Just uid
    uidS _ = Nothing

    gidS (Exec _ gid _) = Just gid
    gidS _ = Nothing

    envS (Environment env) = Just $ M.fromList env
    envS _ = Nothing

    mpS (MountPoints mp) = Just $ map (\(n,p,ro) -> MountPoint n p ro) mp
    mpS _ = Nothing

    portsS (Ports p) = Just $ map (\(n,proto,port,sa) -> Port n proto port sa) p
    portsS _ = Nothing

sectionParser :: Context -> Parser Section
sectionParser ctx = do
    kind <- takeTill isEndOfLine
    case kind of
        "name" -> do
            skipSpace
            Name <$> takeTill isEndOfLine

        "baseImage" -> do
            skipSpace
            BaseImage <$> takeTill isEndOfLine

        "buildInstructions" -> do
            bis <- many' $ do
                skipSpace
                bik <- takeWhile1 (not . isSpace)
                skipSpace
                -- trace ("bik " ++ show bik) $ return ()
                case bik of
                    "run" -> Run <$> commandParser ctx
                    "spawn" -> do
                        string "["
                        bindings <- sepBy' (bindingParser ctx) (string ",")
                        string "]"

                        Spawn <$> pure bindings <*> (commandParser ctx)
                    _ -> fail $ "Unknown build instruction kind " ++ show bik

            pure $ BuildInstructions bis

        "app" -> do
            AppS <$> many' (appDefParser ctx)

        _ -> do
            fail $ "Unknown kind " ++ show kind


token :: Context -> Parser Text
token ctx = do
    skipWhile isHorizontalSpace
    str <- takeWhile1 (not . isSpace)
    pure $ render ctx str

commandParser :: Context -> Parser Cmd
commandParser ctx@Context{..} = do
    binary <- token ctx
    -- trace ("binary " ++ show binary) $ return ()
    args <- takeTill isEndOfLine
    let args' = map (render ctx) $ T.splitOn " " $ T.strip args
    -- trace ("args " ++ show args) $ return ()
    pure $ Cmd binary args'

bindingParser :: Context -> Parser Binding
bindingParser ctx = do
    string "("
    skipSpace
    hostPath <- takeTill (==',')
    -- trace (show hostPath) $ return ()
    skipSpace
    string ","
    skipSpace
    containerPath <- takeTill (==')')
    -- trace (show containerPath) $ return ()
    skipSpace
    string ")"

    -- trace (show hostPath) $ return ()
    pure $ Binding
        (render ctx hostPath)
        containerPath


appDefParser :: Context -> Parser AppDef
appDefParser ctx = do
    skipSpace
    tok <- token ctx
    -- trace (show tok) $ return ()
    case tok of
        "exec" -> do
            skipSpace
            uid <- token ctx
            gid <- token ctx
            cmd <- takeTill isEndOfLine
            Exec <$> pure uid <*> pure gid <*> pure (T.splitOn " " $ T.strip cmd)

        "environment" -> do
            env <- many1' $ do
                skipSpace
                name <- token ctx
                skipSpace
                string "\""
                value <- takeTill (=='"')
                string "\""
                endOfLine

                pure (name,value)

            pure $ Environment env

        "mountPoints" -> do
            mp <- many1 $ do
                skipSpace
                name <- token ctx
                skipWhile (==' ')
                path <- token ctx
                c <- peekChar'
                ro <- if isEndOfLine c
                    then pure False
                    else do
                        ro <- token ctx
                        pure $ case ro of
                            "ro" -> True
                            _    -> False

                endOfLine

                pure $ (name,path,ro)

            pure $ MountPoints mp

        "ports" -> do
            p <- many1 $ do
                skipSpace
                name <- token ctx
                skipSpace
                proto <- takeTill (=='/')
                string "/"
                portNumber <- decimal
                c <- peekChar'
                socketActivated <- if isEndOfLine c
                    then pure False
                    else do
                        sa <- token ctx
                        pure True

                pure $ (name,proto,portNumber,socketActivated)

            pure $ Ports p

        _ ->
            fail $ "Unknown app def: " ++ show tok

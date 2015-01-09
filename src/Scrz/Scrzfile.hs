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
import Control.Monad
import Data.AppContainer.Types
import Data.Attoparsec.Text as AP
import Data.Monoid
import Data.Char
-- import Data.Maybe
import Scrz.Types
import qualified Data.Map as M
import System.Directory (getCurrentDirectory)


import           Data.Text (Text)
import qualified Data.Text as T

-- import Debug.Trace


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
        sectionParser ctx

    -- trace (show sections) $ return ()

    Scrzfile
        <$> findSection "name" sections nameS
        <*> findSection "base image" sections baseImageS
        <*> findSection "build instructions" sections buildInstructionsS
        <*> findSection "app" sections appDefS

  where

    findSection :: String -> [Section] -> (Section -> Maybe a) -> Parser a
    findSection n [] _ = fail $ "Section not found: " <> n
    findSection n (x:xs) f = case f x of
        Nothing -> findSection n xs f
        Just s -> pure s

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


    findAppDef :: Text -> [AppDef] -> (AppDef -> Maybe a) -> Maybe a
    findAppDef _ [] _ = Nothing
    findAppDef n (x:xs) f = case f x of
        Nothing -> findAppDef n xs f
        Just ad -> Just ad

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


blankLine :: Parser ()
blankLine = do
    skipHorizontalSpace
    endOfLine

skipHorizontalSpace :: Parser ()
skipHorizontalSpace = do
    void $ AP.takeWhile isHorizontalSpace

sectionParser :: Context -> Parser Section
sectionParser ctx = do
    void $ many' blankLine

    kind <- takeTill isEndOfLine
    endOfLine
    case kind of
        "name" -> do
            skipHorizontalSpace
            Name <$> takeTill isEndOfLine <* endOfLine

        "baseImage" -> do
            skipHorizontalSpace
            BaseImage <$> takeTill isEndOfLine <* endOfLine

        "buildInstructions" -> do
            bis <- many' $ do
                skipHorizontalSpace
                bik <- takeWhile1 (not . isSpace) -- a token?
                skipHorizontalSpace
                -- trace ("bik " ++ show bik) $ return ()
                case bik of
                    "run" -> Run <$> commandParser ctx <* endOfLine
                    "spawn" -> do
                        void $ char '['
                        bindings <- sepBy' (bindingParser ctx) (char ',')
                        void $ char ']'

                        Spawn <$> pure bindings <*> (commandParser ctx) <* endOfLine
                    _ -> fail $ "Unknown build instruction kind " ++ show bik

            pure $ BuildInstructions bis

        "app" -> do
            AppS <$> many' (appDefParser ctx)

        _ -> do
            fail $ "Unknown kind " ++ show kind


token :: Context -> Parser Text
token ctx = do
    skipWhile isHorizontalSpace
    str <- takeWhile1 (not . isHorizontalSpace)
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
    void $ char '('
    skipHorizontalSpace
    hostPath <- takeTill (==',')
    -- trace (show hostPath) $ return ()
    skipHorizontalSpace
    void $ char ','
    skipHorizontalSpace
    containerPath <- takeTill (==')')
    -- trace (show containerPath) $ return ()
    skipHorizontalSpace
    void $ char ')'

    -- trace (show hostPath) $ return ()
    pure $ Binding
        (render ctx hostPath)
        containerPath


appDefParser :: Context -> Parser AppDef
appDefParser ctx = do
    skipHorizontalSpace
    tok <- token ctx
    -- trace (show tok) $ return ()
    case tok of
        "exec" -> do
            skipHorizontalSpace
            uid <- token ctx
            gid <- token ctx
            cmd <- takeTill isEndOfLine
            Exec <$> pure uid <*> pure gid <*> pure (T.splitOn " " $ T.strip cmd)
                <* endOfLine

        "environment" -> do
            env <- many1' $ do
                skipHorizontalSpace
                name <- token ctx
                skipHorizontalSpace
                void $ char '"'
                value <- takeTill (=='"')
                void $ char '"'
                endOfLine

                pure (name,value)

            pure $ Environment env

        "mountPoints" -> do
            mp <- many1 $ do
                skipHorizontalSpace
                name <- token ctx
                skipWhile isHorizontalSpace
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
                skipHorizontalSpace
                name <- token ctx
                skipHorizontalSpace
                proto <- takeTill (=='/')
                void $ char '/'
                portNumber <- decimal
                c <- peekChar'
                socketActivated <- if isEndOfLine c
                    then pure False
                    else do
                        skipHorizontalSpace
                        void $ string "socketActivated"
                        pure True
                endOfLine

                pure $ (name,proto,portNumber,socketActivated)

            pure $ Ports p

        _ ->
            fail $ "Unknown app def: " ++ show tok

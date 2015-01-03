{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where


import Control.Monad
import Control.Monad.Except

import Control.Exception
import Control.Concurrent

import Scrz.Log
import Scrz.Utils
import Scrz.Etcd
import Scrz.Host
import Scrz.Types
import Network.Etcd

import Data.Monoid
import Data.Maybe
import Data.UUID
import qualified Data.Text as T
import Options.Applicative



data Options = Options !Command

data Command
    = Version
    | Run


main :: IO ()
main = do

    disableOutputBuffering

    md <- runExceptT $ do
        client <- scrzIO $ createClient ["http://localhost:4001"]

        mId <- machineId
        hostName <- fromMaybe "localhost" <$> fullyQualifiedDomainName

        updateMachineDescription client $ Machine mId hostName

    case md of
        Left e -> error $ show e
        Right _ -> return ()

    run =<< execParser
        (parseOptions `withInfo` "scrz node supervisor")


run :: Options -> IO ()
run (Options Version) = do
    putStrLn "v0.0.1"

run (Options Run) = do
    putStrLn "running node supervisor..."

    forever $ do
        syncContainerSupervisors `catch` \(e :: SomeException) -> do
            logger $ "Syncing with remote authority failed: " ++ show e

        threadDelay delay


  where

    delay = 10 * 1000 * 1000

    syncContainerSupervisors = do
        res <- runExceptT $ do
            mId <- machineId
            client <- scrzIO $ createClient ["http://localhost:4001"]
            listContainerIds client mId

        case res of
            Left e -> error $ show e
            Right keys -> forM_ keys $ \cId -> do
                let key = toString $ unContainerId cId
                exec "systemctl" ["start", "scrz-container-supervisor@" <> key <> ".service"] >>= fatal
                return ()


parseOptions :: Parser Options
parseOptions = Options <$> parseCommand

parseCommand :: Parser Command
parseCommand = (subparser $ mconcat
    [ command "version"
        (parseVersion `withInfo` "Print the version and exit")
    ]) <|> pure Run

parseVersion :: Parser Command
parseVersion = pure Version

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

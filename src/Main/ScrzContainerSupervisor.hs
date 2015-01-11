{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Main where


import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Concurrent
import System.Directory

import Scrz.Image
import Scrz.Types
import Scrz.Utils
import Scrz.Host
import Scrz.Etcd

import Network.Etcd
import Data.AppContainer.Types
import Data.Aeson
import Data.Maybe
import qualified Data.ByteString.Lazy as LB
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Text.Encoding
import Data.UUID
import Data.List

import Data.Monoid
import Options.Applicative
import Options.Applicative.Types



data Options = Options !Command

data Command
    = Version
    | Run !ContainerId


main :: IO ()
main = do

    disableOutputBuffering

    run =<< execParser
        (parseOptions `withInfo` "scrz container supervisor")


run :: Options -> IO ()
run (Options Version) = do
    putStrLn "v0.0.1"

run (Options (Run cId)) = do
    putStrLn $ "Running container supervisor " <> show (unContainerId cId) <> "..."

    mbContainerManifest <- runExceptT $ lookupContainerManifest cId
    case mbContainerManifest of
        Left e -> do
            print e
            putStrLn $ "Container runtime manifest is not available. Shutting down..."

        Right crm@ContainerRuntimeManifest{..} -> do
            print crmUUID

            let containerPath = "/var/lib/scrz/containers/" ++ Data.UUID.toString crmUUID
                containerRootfs = containerPath ++ "/rootfs"

            createDirectoryIfMissing True containerPath

            let img = head crmImages
            fir <- runExceptT $ fetchImage (imageApp img)
            (iId, im@ImageManifest{..}) <- case fir of
                Left e -> error $ show e
                Right x -> return x

            bindings <- buildBindings crm im

            void $ runExceptT $ btrfsSubvolSnapshot
                ("/var/lib/scrz/images/" <> T.unpack iId <> "/rootfs")
                (containerRootfs)

            let App{..} = fromJust imApp

            let args = [ "-D", containerRootfs, "-M", Data.UUID.toString crmUUID, "-j"
                       ] ++ map (\(src, path) -> "--bind=" <> T.unpack src <> ":" <> T.unpack path) bindings
                         ++ map (\(k, v) -> "--setenv=" <> T.unpack k <> ":" <> T.unpack v) (M.toList appEnvironment)
                         ++ ["--"]
                         ++ map T.unpack appExec

            p <- exec "systemd-nspawn" args

            void $ forkIO $ forever $ do
                mbCM <- runExceptT $ lookupContainerManifest cId
                case mbCM of
                    Right _ -> threadDelay $ 1000 * 1000
                    Left _ -> do
                        putStrLn $ "Container no longer active, killing..."
                        kill p

            putStrLn $ "Waiting for systemd-nspawn to exit... "
            void $ wait p

            putStrLn $ "Application exited, cleaning up..."

            void $ runExceptT $ btrfsSubvolDelete containerRootfs

            -- TODO: Clean up 'containerPath'


buildBindings :: ContainerRuntimeManifest -> ImageManifest -> IO [(Text, Text)]
buildBindings crm@ContainerRuntimeManifest{..} ImageManifest{..} = do
    let Just App{..} = imApp

    -- imMountPoints are the mount points which the container manifest must
    -- fulfill.
    --
    -- crmVolumes defines how these mount points are fulfilled.

    forM appMountPoints $ \MountPoint{..} -> do
        mbVol <- findMountPointVolume crm mpName
        case mbVol of
            Nothing -> error $ "Could not satisfy mount point " ++ show mpName
            Just Volume{..} -> do
                let HostVolumeSource HostVolume{..} = volSource
                return (hvSource, mpPath)


findMountPointVolume :: ContainerRuntimeManifest -> Text -> IO (Maybe Volume)
findMountPointVolume ContainerRuntimeManifest{..} mp = do
    return $ (flip find) crmVolumes $ \Volume{..} ->
        mp `elem` volFulfills


parseOptions :: Parser Options
parseOptions = Options <$> parseCommand

parseCommand :: Parser Command
parseCommand = (subparser $ mconcat
    [ command "version"
        (parseVersion `withInfo` "Print the version and exit")
    ]) <|> (Run <$> argument containerIdRead (metavar "CONTAINER"))

parseVersion :: Parser Command
parseVersion = pure Version

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc



localEtcdServer :: [Text]
localEtcdServer = ["http://localhost:4001"]

containerIdRead :: ReadM ContainerId
containerIdRead = ReadM $ do
    val <- ask
    case fromString val of
        Nothing -> fail "Not an UUID"
        Just v  -> return $ ContainerId v


lookupContainerManifest :: ContainerId -> Scrz ContainerRuntimeManifest
lookupContainerManifest cId = do
    mId <- machineId
    client <- liftIO $ createClient localEtcdServer
    mbNode <- liftIO $ get client (containerManifestKey mId cId)
    case mbNode of
        Nothing -> throwError $ InternalError "Node not found"
        Just node -> case eitherDecode (rValueLB node) of
            Left e -> throwError $ InternalError $ T.pack e
            Right x -> return x

rValueLB :: Node -> LB.ByteString
rValueLB response = LB.fromStrict $ encodeUtf8 $ fromJust $ _nodeValue response

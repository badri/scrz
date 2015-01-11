{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Scrz.Container
    ( runContainer
    ) where


import           Control.Applicative
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Concurrent

import           System.Directory
import           System.Process

import           Data.List
import           Data.UUID
import           Data.Monoid
import           Data.Maybe

import           Data.Text (Text)
import qualified Data.Text as T

import qualified Data.Map as M

import           Data.AppContainer.Types

import           System.Directory
import           System.FilePath
import           System.IO

import           Network.BSD (getHostName)
import           Network.Socket

import           Scrz.Types
import           Scrz.Utils
import           Scrz.Image
import           Scrz.Host




runContainer :: ContainerId -> ContainerRuntimeManifest -> Scrz ProcessHandle
runContainer cId crm@ContainerRuntimeManifest{..} = do
    scrzIO $ putStrLn $ "ContainerId " <> show crmUUID <> "..."

    let containerPath = "/var/lib/scrz/containers/" ++ Data.UUID.toString crmUUID
        containerRootfs = containerPath ++ "/rootfs"

    scrzIO $ createDirectoryIfMissing True containerPath

    let img = head crmImages

    scrzIO $ putStrLn "Downloading image..."
    (iId, im@ImageManifest{..}) <- fetchImage (imageApp img)

    bindings <- scrzIO $ buildBindings crm im

    void $ btrfsSubvolSnapshot
        ("/var/lib/scrz/images/" <> T.unpack iId <> "/rootfs")
        (containerRootfs)

    let App{..} = fromJust imApp

    let args = [ "-D", containerRootfs, "-M", Data.UUID.toString crmUUID, "-j"
               ] ++ map (\(src, path) -> "--bind=" <> T.unpack src <> ":" <> T.unpack path) bindings
                 ++ map (\(k, v) -> "--setenv=" <> T.unpack k <> "=" <> T.unpack v) (M.toList appEnvironment)
                 ++ ["--"]
                 ++ map T.unpack appExec

    -- scrzIO $ putStrLn $ "nspawn command: " ++ show args
    scrzIO $ exec "systemd-nspawn" args


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

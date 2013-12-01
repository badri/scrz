module Scrz.Container
  (

    createContainer
  , startContainer
  , stopContainer
  , destroyContainer

  ) where

import Data.Maybe
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Time.Clock

import System.Directory
import System.IO

import Control.Monad

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM

import Scrz.Types
import Scrz.Utils
import Scrz.Image
import Scrz.LXC
import Scrz.Network
import Scrz.Volume
import Scrz.Log

createContainer :: TVar Runtime -> Authority -> Service -> Image -> IO (TVar Container)
createContainer runtime authority service@Service{..} image = do
    now <- getCurrentTime
    id' <- newId


 -- Make sure the image is downloaded and ready to use.
    ensureImage image


 -- Allocate runtime resources (address, ports, volumes etc).
    addr <- allocateAddress runtime serviceAddress
    externalPorts <- allocatePorts runtime addr servicePorts
    backingVolumes' <- allocateVolumes runtime serviceVolumes


 -- Prepare the filesystem (clone image, write LXC config file).
    let containerPath  = "/srv/scrz/containers/" ++ id'
    let rootfsPath     = containerPath ++ "/rootfs"
    let lxcConfigPath  = containerPath ++ "/config"
    gatewayAddress <- atomically $ bridgeAddress <$> readTVar runtime

    cloneImage image rootfsPath

    let volumes = zip backingVolumes' serviceVolumes
    writeFile lxcConfigPath $ lxcConfig id' addr gatewayAddress rootfsPath volumes


 -- Create directories for the mount points
    forM_ serviceVolumes $ \volume -> do
        let mountPoint = rootfsPath ++ (volumePath volume)
        createDirectoryIfMissing True mountPoint


 -- Update container config files (/etc/hosts, /etc/portmap, ...)
    let hostsLine = ["127.0.0.1", id', "localhost" ]
    writeFile (rootfsPath ++ "/etc/hosts") $ intercalate " " hostsLine

    let ports   = zip servicePorts externalPorts
    let portmap = map (\(int, ext) -> (show $ internalPort int) ++ "=" ++ (show ext)) ports
    writeFile (rootfsPath ++ "/etc/portmap") $ intercalate " " portmap

    -- TODO: Do something sensible when the FQDN isn't known.
    fqdn <- fullyQualifiedDomainName
    writeFile (rootfsPath ++ "/etc/scrz-host-domain") $ fromMaybe "" fqdn


 -- Register the container in the runtime.
    container <- newTVarIO $ Container id' now authority service image addr externalPorts backingVolumes' Nothing
    atomically $ modifyTVar runtime $ \x ->
        x { containers = M.insert id' container (containers x) }

    return container


startContainer :: TVar Container -> Maybe Handle -> IO ()
startContainer container mbHandle = do
    Container{..} <- atomically $ readTVar container
    let Service{..} = containerService

    let containerPath = "/srv/scrz/containers/" ++ containerId
    let lxcConfigPath = containerPath ++ "/config"

    unless (isJust containerProcess) $ do
        let args = [ "-n", containerId
                   , "-f", lxcConfigPath
                   , "-c", "/dev/null"
                   , "--"
                   , "/sbin/scrz-init"
                   ] ++ serviceCommand

        p <- execEnv "lxc-start" args [] mbHandle
        void $ forkFinally (waitE p) clearContainerProcess

        atomically $ modifyTVar container $ \x ->
            x { containerProcess = Just p }

  where

    clearContainerProcess = const $ do
        logger "lxc-start exited"
        when (isJust mbHandle) $ do
            logger "Closing slave handle"
            hClose (fromJust mbHandle)
        atomically $ modifyTVar container $ \x ->
            x { containerProcess = Nothing }


stopContainer :: TVar Container -> IO ()
stopContainer container = do
    Container{..} <- atomically $ readTVar container

    when (isJust containerProcess) $ do
        exec "lxc-stop" [ "-n", containerId ] >>= wait
        waitE (fromJust containerProcess)

        atomically $ modifyTVar container $ \x ->
            x { containerProcess = Nothing }

    return ()


-- | Release all resources (IP addresses, mapped, rootfs clone etc) used by
--   the container and remove it form the runtime.
destroyContainer :: TVar Runtime -> TVar Container -> IO ()
destroyContainer runtime container = do
    Container{..} <- atomically $ readTVar container
    let Service{..} = containerService

 -- Release runtime resources.
    releasePorts runtime containerAddress servicePorts containerPorts
    releaseAddress runtime containerAddress
    releaseVolumes runtime containerVolumes


 -- Delete resources from the filesystem.
    let containerPath = "/srv/scrz/containers/" ++ containerId
    let rootfsPath    = containerPath ++ "/rootfs"

    deleteImageClone rootfsPath
    removeDirectoryRecursive containerPath


 -- Unregister the container from the runtime.
    atomically $ modifyTVar runtime $ \x ->
        x { containers = M.delete containerId (containers x) }

module Scrz.Supervisor (startSupervisor) where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS

import Control.Applicative
import Control.Monad
import System.Directory

import Control.Exception
import Control.Concurrent
import Control.Concurrent.STM

import Network.Socket

import Scrz.Container
import Scrz.Image
import Scrz.Log
import Scrz.Network
import Scrz.Signal
import Scrz.Socket
import Scrz.Types
import Scrz.Utils
import Scrz.Etcd


startSupervisor :: Maybe String -> IO ()
startSupervisor remoteAuthorityUrl = do
    runtime <- newTVarIO =<< initializeRuntime

    mvar <- newEmptyTMVarIO
    controlThread <- createControlThread mvar runtime remoteAuthorityUrl
    setupSignalHandlers controlThread

    -- Wait for the control thread to finish.
    atomically $ takeTMVar mvar

    rt <- atomically $ readTVar runtime
    mapM_ (\x -> stopContainer x >> destroyContainer runtime x) $ M.elems (containers rt)

    cleanupNetwork


initializeRuntime :: IO Runtime
initializeRuntime = do
    (bridgeAddress', networkAddresses', networkPorts') <- initializeNetwork

    return $ Runtime
      { bridgeAddress     = bridgeAddress'
      , networkAddresses  = networkAddresses'
      , networkPorts      = networkPorts'
      , backingVolumes    = M.empty
      , containers        = M.empty
      }


createControlThread :: TMVar () -> TVar Runtime -> Maybe String -> IO ThreadId
createControlThread mvar runtime remoteAuthorityUrl = do
    void $ forkIO $ forever $ loadLocalConfig >> threadDelay delay

    withMaybe remoteAuthorityUrl $ \url -> do
        void $ forkIO $ forever $ do
            syncRemoteConfig url `catch` \(e :: SomeException) -> do
                logger $ "Syncing with remote authority failed: " ++ show e

            threadDelay delay

    sock <- serverSocket
    forkFinally (forever $ handleClient runtime sock) (cleanup sock)


  where

    delay = 10 * 1000 * 1000

    cleanup :: Socket -> Either SomeException () -> IO ()
    cleanup sock ex = do
        logger $ show ex
        close sock
        removeFile controlSocketPath
        atomically $ putTMVar mvar ()

    loadLocalConfig = do
        conf <- LBS.readFile "/etc/scrz/config.json"

        case A.decode conf :: Maybe Config of
            Nothing -> putStrLn "Could not decode config"
            Just config -> mergeConfig runtime Local config

    syncRemoteConfig url = do
        services <- listServices
        mergeConfig runtime (Remote url) (Config services)

        rt <- atomically $ readTVar runtime
        updateRuntime rt


mergeConfig :: TVar Runtime -> Authority -> Config -> IO ()
mergeConfig runtime authority config = do
    -- Remove old services from the local runtime.
    rt <- atomically $ readTVar runtime
    forM_ (M.elems $ containers rt) $ \container -> do
        c <- atomically $ readTVar container

        let matchAuthority = authority == containerAuthority c
        let hasService = L.elem (containerService c) (configServices config)

        when (matchAuthority && not hasService) $ do
            logger $ "Service removed from the configuration, stopping container"
            stopContainer container
            destroyContainer runtime container

    -- Add new services from the authority.
    forM_ (configServices config) addService

  where

    addService :: Service -> IO ()
    addService service = do
        rt <- atomically $ readTVar runtime
        exists <- hasContainer rt authority service

        unless exists $ do
            let image = imageFromMeta $ serviceImage service
            container <- createContainer runtime authority service image
            startContainer container Nothing
            id' <- atomically $ containerId <$> readTVar container
            logger $ show id'


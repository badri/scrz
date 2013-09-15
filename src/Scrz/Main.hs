module Main where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import           Data.Time.Format.Human

import Control.Applicative
import Control.Monad
import System.Environment
import System.Directory
import System.Posix.Process
import System.Posix.IO
import System.Posix.Terminal (openPseudoTerminal, getSlaveTerminalName)

import Control.Exception
import Control.Concurrent
import Control.Concurrent.STM

import Network.Socket

import Scrz.Protocol
import Scrz.Container
import Scrz.Http
import Scrz.Image
import Scrz.Log
import Scrz.Network
import Scrz.Proxy
import Scrz.Signal
import Scrz.Socket
import Scrz.Terminal
import Scrz.Types
import Scrz.Utils
import Scrz.Etcd

withMaybe :: Maybe a -> (a -> IO ()) -> IO ()
withMaybe Nothing  _ = return ()
withMaybe (Just a) f = f a

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


run :: [ String ] -> IO ()
run [ "supervisor" ] = startSupervisor Nothing
run [ "supervisor", remoteAuthorityUrl ] = startSupervisor $ Just remoteAuthorityUrl

run [ "list-containers" ] = do
    void $ sendCommand ListContainers >>= printResponse

run [ "ps" ] = do
    void $ sendCommand ListContainers >>= printResponse

run [ "inspect", id' ] = do
    ListContainersResponse containers <- sendCommand ListContainers
    let Just container = L.find (\x -> id' == containerId x) containers

    let Container{..} = container
    let Service{..}   = containerService
    let Image{..}     = containerImage
    let ImageMeta{..} = maybe (ImageMeta "" "" 0) id imageMeta

    timeDiff <- humanReadableTime containerCreatedAt

    tabWriter [ [ "ID", containerId ]
              , [ "CREATEDAT", timeDiff ]
              , [ "IMAGE", imageId ++ " (" ++ imageUrl ++ ")" ]
              , [ "ADDRESS", show containerAddress ]
              , [ "COMMAND", L.intercalate " " serviceCommand ]
              , [ "PORTS", L.intercalate " " (map showPort (zip containerPorts servicePorts)) ]
              , [ "VOLUMES", L.intercalate " " (map showVolume (zip containerVolumes serviceVolumes)) ]
              ]

    return ()

  where

    showPort :: (Int, Port) -> String
    showPort (ext, Port{..}) = show ext ++ "=" ++ show internalPort

    showVolume :: (BackingVolume, Volume) -> String
    showVolume (backingVolume, Volume{..}) =
        volumePath ++ " -> " ++ backingVolumePath backingVolume

run [ "stop-container", id' ] = do
    void $ sendCommand $ StopContainer id'

run [ "destroy-container", id' ] = do
    void $ sendCommand $ DestroyContainer id'

run [ "start", id' ] = do
    void $ sendCommand $ Start id'

run [ "stop", id' ] = do
    void $ sendCommand $ StopContainer id'

run [ "restart", id' ] = do
    void $ sendCommand $ StopContainer id'
    void $ sendCommand $ Start id'

run [ "snapshot", container, image ] = do
    void $ sendCommand $ Snapshot container image

run [ "quit" ] = do
    void $ sendCommand $ Quit

run [ "console", id' ] = do
    executeFile "lxc-console" True [ "-n", id' ] Nothing

run [ "list-images" ] = do
    images <- loadImages

    let headers = [ "ID", "SIZE", "CSUM",  "URL" ]
    rows <- mapM toRow $ M.toList images
    tabWriter $ headers : rows

  where

    toRow :: (String, Image) -> IO [String]
    toRow (localImageId, image) = do
        let meta = imageMeta image
        ok <- maybe (return "-") (const $ verifyContent image "✓" $ \_ _ -> return "✗") meta

        return [ localImageId
               , maybe "-" (show . imageSize) meta
               , ok
               , maybe "-" imageUrl (imageMeta image)
               ]


run [ "download-image", url, checksum, sizeStr ] = do
    let image = imageFromMeta $ ImageMeta url checksum $ read sizeStr
    ensureImage image
    logger $ "Image available under id " ++ imageId image

run [ "destroy-image", localImageId ] = do
    destroyImage localImageId

run [ "clone-image", localImageId, newImageId ] = do
    let srcImage = Image localImageId Nothing
    let dstImage = Image newImageId Nothing

    createDirectoryIfMissing True (imageBasePath dstImage)
    cloneImage srcImage (imageVolumePath dstImage)


run [ "ipvs", addr, url ] = do
    runtime <- mkProxyRuntime addr

    forever $ do
        putStrLn "arst"
        syncProxyConfig runtime `catch` \(e :: SomeException) -> do
            logger $ "Syncing proxy config with remote authority failed: " ++ show e

        threadDelay $ 10 * 1000 * 1000

  where

    syncProxyConfig runtime = do
        fqdn' <- fullyQualifiedDomainName
        withMaybe fqdn' $ \fqdn -> do
            config' <- getJSON $ url ++ "/api/hosts/" ++ fqdn ++ "/proxy-config"
            print config'
            withMaybe config' $ \config -> do
                mergeProxyConfig runtime config


run [ "pack-image", id' ] = do
    packImage id'

run ("run":args) = do
    (ptm, pts) <- openPseudoTerminal
    attrs      <- setRawModeFd stdInput

    response <- finally (sendRunCommand ptm) (freeResources ptm pts attrs) `catch` \(_ :: SomeException) -> return ErrorResponse

    logger $ show response
    handleResponse response `onException` do
        logger $ "Got exception"


  where
    ra = parseRunArguments (RunArgs "" [] [] Nothing) args
    pump src dst = fdRead src 999 >>= \(x, _) -> fdWrite dst x

    freeResources ptm pts attrs = do
        resetModeFd stdInput attrs
        closeFd ptm
        closeFd pts

    sendRunCommand ptm = do
        void $ forkFinally (forever $ pump ptm stdOutput) (const $ return ())
        void $ forkFinally (forever $ pump stdInput ptm)  (const $ return ())

        slaveName <- getSlaveTerminalName ptm
        response  <- sendCommand $ Run (runArgsImage ra) (runArgsCommand ra) slaveName (runArgsMounts ra)

        case response of
            CreateContainerResponse id' -> void $ sendCommand $ Wait id'
            _ -> return ()

        return response

    handleResponse response = do
        case response of
            CreateContainerResponse id' -> do
                imageId' <- maybe newId return (runArgsSaveAs ra)
                logger $ "Saving image under id " ++ imageId'
                void $ sendCommand $ Snapshot id' imageId'
                void $ sendCommand $ DestroyContainer id'

            _ -> do
                logger $ "Received unexpected response: " ++ show response


run args = do
    logger $ "Unknown arguments: " ++ (show args)


data RunArgs = RunArgs
  { runArgsImage :: String
  , runArgsCommand :: [String]
  , runArgsMounts :: [(String,String)]
  , runArgsSaveAs :: Maybe String
  } deriving (Show)

parseRunArguments :: RunArgs -> [String] -> RunArgs
parseRunArguments ra ("--save-as" : id' : args) =
    let pra = ra { runArgsSaveAs = Just id' }
    in parseRunArguments pra args

parseRunArguments ra ("--mount" : bv : mp : args) =
    let pra = ra { runArgsMounts = (bv,mp) : runArgsMounts ra }
    in parseRunArguments pra args

parseRunArguments ra (image : command) =
    ra { runArgsImage = image, runArgsCommand = command }

parseRunArguments ra [] = ra

main :: IO ()
main = getArgs >>= run

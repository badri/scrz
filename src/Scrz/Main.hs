module Main where

import Control.Monad
import System.Environment
import System.Directory
import System.Posix.Process
import System.Posix.IO
import System.Posix.Terminal (openPseudoTerminal, getSlaveTerminalName)

import Control.Exception
import Control.Concurrent

import Scrz.Protocol
import Scrz.Commands
import Scrz.Image
import Scrz.Log
import Scrz.Proxy
import Scrz.Socket
import Scrz.Terminal
import Scrz.Types
import Scrz.Utils
import Scrz.Etcd
import Scrz.Supervisor



run :: [ String ] -> IO ()

run [ "supervisor" ]                 = startSupervisor Nothing
run [ "supervisor", url ]            = startSupervisor (Just url)

run [ "ipvs", addr, url ]            = ipvsProxy addr url

run [ "inspect", id' ]               = inspectContainer id'
run [ "ps" ]                         = listContainers
run [ "list-containers" ]            = listContainers
run [ "stop-container", id' ]        = stopContainer id'
run [ "destroy-container", id' ]     = destroyContainer id'
run [ "start", id' ]                 = startContainer id'
run [ "stop", id' ]                  = stopContainer id'
run [ "restart", id' ]               = stopContainer id' >> startContainer id'
run [ "quit" ]                       = quitSupervisor
run [ "snapshot", container, image ] = snapshotContainer container image
run [ "pack-image", id' ]            = packImage id'
run [ "list-images" ]                = listImages
run [ "destroy-image", id' ]         = destroyImage id'
run [ "download-image", url, checksum, size ] = downloadImage url checksum (read size)
run [ "update-service-image", etcdHost, host, service, image, url] = updateServiceImage etcdHost host service image url


run [ "console", id' ] = do
    executeFile "lxc-console" True [ "-n", id' ] Nothing


run [ "clone-image", localImageId, newImageId ] = do
    let srcImage = Image localImageId Nothing
    let dstImage = Image newImageId Nothing

    createDirectoryIfMissing True (imageBasePath dstImage)
    cloneImage srcImage (imageVolumePath dstImage)


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

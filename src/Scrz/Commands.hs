-- | The client-side implementations of most CLI commands is in this module.

module Scrz.Commands where

import qualified Data.Map  as M
import qualified Data.List as L
import           Data.Time.Format.Human

import Control.Monad
import Scrz.Socket
import Scrz.Protocol
import Scrz.Log
import Scrz.Image
import Scrz.Types


listContainers :: IO ()
listContainers = do
    void $ sendCommand ListContainers >>= printResponse


stopContainer :: String -> IO ()
stopContainer containerId = do
    void $ sendCommand $ StopContainer containerId


destroyContainer :: String -> IO ()
destroyContainer containerId = do
    void $ sendCommand $ DestroyContainer containerId

snapshotContainer :: String -> String -> IO ()
snapshotContainer containerId imageId = do
    void $ sendCommand $ Snapshot containerId imageId


startContainer :: String -> IO ()
startContainer containerId = do
    void $ sendCommand $ Start containerId


downloadImage :: String -> String -> Int -> IO ()
downloadImage url checksum size = do
    let image = imageFromMeta $ ImageMeta url checksum size
    ensureImage image
    logger $ "Image available under id " ++ imageId image


quitSupervisor :: IO ()
quitSupervisor = do
    void $ sendCommand $ Quit


listImages :: IO ()
listImages = do
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

inspectContainer :: String -> IO ()
inspectContainer id' = do
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

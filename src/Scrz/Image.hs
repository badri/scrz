module Scrz.Image where

import           Data.Map (Map)
import qualified Data.Map as M

import System.Directory
import System.FilePath

import Control.Monad
import Control.Applicative
import Control.Concurrent.STM

import Scrz.Types
import Scrz.Utils
import Scrz.Http


baseImageDirectory :: String
baseImageDirectory = "/srv/scrz/images"

imageBasePathS :: String -> String
imageBasePathS = (</>) baseImageDirectory

imageBasePath :: Image -> String
imageBasePath = imageBasePathS . imageId

imageContentPathS :: String -> String
imageContentPathS image = imageBasePathS image </> "content"

imageContentPath :: Image -> String
imageContentPath = imageContentPathS . imageId

imageVolumePathS :: String -> String
imageVolumePathS image = imageBasePathS image </> "volume"

imageVolumePath :: Image -> String
imageVolumePath = imageVolumePathS . imageId

getImage :: String -> IO Image
getImage id' = do
    images <- loadImages
    case M.lookup id' images of
        Nothing -> error "Image not found"
        Just x -> return x


loadImages :: IO (Map String Image)
loadImages = do
    images <- getDirectoryContents baseImageDirectory
    return $ M.fromList $ map (\id' -> (id', Image id' "" 0)) images


cloneImage :: Image -> String -> IO ()
cloneImage image path = do

    p <- exec "btrfs" [ "subvolume", "snapshot", imageVolumePath image, path ]
    fatal p


deleteImageClone :: String -> IO ()
deleteImageClone path = do
    p <- exec "btrfs" [ "subvolume", "delete", path ]
    wait p


snapshotContainerImage :: TVar Container -> String -> IO ()
snapshotContainerImage container image = do
    id' <- atomically $ containerId <$> readTVar container
    let rootfsPath = "/srv/scrz/containers/" ++ id' ++ "/rootfs"

    createDirectoryIfMissing True imagePath
    p <- exec "btrfs" [ "subvolume", "snapshot", rootfsPath, volumePath']
    wait p

  where

    imagePath   = "/srv/scrz/images/" ++ image
    volumePath' = imagePath ++ "/volume"


ensureImage :: Image -> IO ()
ensureImage image = do
    imageContentExists <- doesFileExist $ imageContentPath image
    unless imageContentExists $ downloadImage image

    imageVolumeExists <- doesDirectoryExist $ imageVolumePath image
    unless imageVolumeExists $ unpackImage image


downloadImage :: Image -> IO ()
downloadImage image = do
    createDirectoryIfMissing True $ imageBasePath image
    downloadBinary (imageUrl image) $ imageContentPath image


unpackImage :: Image -> IO ()
unpackImage image = do
    fatal =<< exec "btrfs" [ "subvolume", "create", imageVolumePath image ]
    fatal =<< exec "tar" [ "-xf", imageContentPath image, "-C", imageVolumePath image ]


packImage :: String -> IO ()
packImage image = do
    fatal =<< exec "tar" [ "-czf", imageContentPathS image, "-C", imageVolumePathS image, "." ]

module Scrz.Image where

import           Data.Aeson

import           Data.Map (Map)
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Base16 as BS16
import qualified Data.ByteString.Lazy as LB

import qualified Crypto.Hash.SHA3 as SHA3

import System.Directory
import System.FilePath

import Control.Monad
import Control.Applicative
import Control.Concurrent.STM

import Scrz.Types
import Scrz.Utils
import Scrz.Http
import Scrz.Log


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

imageMetaPathS :: String -> String
imageMetaPathS image = imageBasePathS image </> "meta"

imageMetaPath :: Image -> String
imageMetaPath = imageMetaPathS . imageId


getImage :: String -> IO Image
getImage id' = do
    images <- loadImages
    case M.lookup id' images of
        Nothing -> error "Image not found"
        Just x -> return x


loadImages :: IO (Map String Image)
loadImages = do
    ids <- filter (\x -> '.' /= head x) <$> getDirectoryContents baseImageDirectory
    images <- mapM loadImageMeta ids
    return $ M.fromList $ zip ids images

  where

    loadImageMeta :: String -> IO Image
    loadImageMeta imgId = do
        metaExists <- doesFileExist $ imageMetaPathS imgId
        if not metaExists
            then return $ Image "-" "-" 0
            else do
                meta <- LB.readFile $ imageMetaPathS imgId
                return $ case decode meta of
                    Nothing -> Image "-" "-" 0
                    Just x -> x

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


verifyContent :: Image -> (String -> Int -> IO ()) -> IO ()
verifyContent image action = do
    (checksum, size) <- hashFile (imageContentPath image)

    let checksumOk = isCorrectChecksum checksum image
    let sizeOk     = isCorrectSize size image

    when (not (checksumOk && sizeOk)) $
        action checksum size


ensureImage :: Image -> IO ()
ensureImage image = do
    imageContentExists <- doesFileExist $ imageContentPath image

    -- Verify the content file if it already exists. If the verification
    -- fails, delete the file and we'll try to download it again.

    when imageContentExists $ do
        logger $ "Verifying existing image content file"
        verifyContent image $ \checksum size -> do
            logger $ "Content file is corrupt, deleting"
            removeFile (imageContentPath image)


    -- Check again if the file exists. It may have been removed if the
    -- verification just above failed.

    imageContentExists <- doesFileExist $ imageContentPath image
    unless imageContentExists $ downloadImage image


    -- FIXME: Only verify if we've downloaded the file.
    verifyContent image $ \checksum size -> do
        logger $ "Downloaded image has invalid checksum"
        error "Image checksum mismatch"


    imageVolumeExists <- doesDirectoryExist $ imageVolumePath image
    unless imageVolumeExists $ unpackImage image


downloadImage :: Image -> IO ()
downloadImage image = do
    logger $ "Downloading image " ++ imageId image ++ " from " ++ imageUrl image
    createDirectoryIfMissing True $ imageBasePath image
    downloadBinary (imageUrl image) $ imageContentPath image
    writeMetaFile (imageId image) image


unpackImage :: Image -> IO ()
unpackImage image = do
    fatal =<< exec "btrfs" [ "subvolume", "create", imageVolumePath image ]
    unpackTarball (imageContentPath image) (imageVolumePath image)


createTarball :: String -> String -> IO ()
createTarball tgz path = do
    fatal =<< exec "tar" [ "-czf", tgz, "-C", path, "." ]


unpackTarball :: String -> String -> IO ()
unpackTarball tgz path = do
    fatal =<< exec "tar" [ "-xf", tgz, "-C", path ]


writeMetaFile :: String -> Image -> IO ()
writeMetaFile localImageId image = do
    LB.writeFile (imageMetaPathS localImageId) $ encode image


readMetaFile :: String -> IO (Maybe Image)
readMetaFile localImageId = do
    metaFileExists <- doesFileExist $ imageMetaPathS localImageId
    if not metaFileExists
        then return Nothing
        else decode <$> LB.readFile (imageMetaPathS localImageId)


hashFile :: String -> IO (String, Int)
hashFile path = do
    contents <- LB.readFile path
    let size = fromIntegral $ LB.length contents
    return $ (BS8.unpack $ BS16.encode $ SHA3.hashlazy 512 contents, size)


packImage :: String -> IO ()
packImage localImageId = do
    createTarball contentPath (imageVolumePathS localImageId)
    (hash, size) <- hashFile contentPath
    writeMetaFile localImageId $ Image localImageId hash size

  where

    contentPath = imageContentPathS localImageId

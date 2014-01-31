
-- | Functions related to handling images.

module Scrz.Image
  (
    -- * Creating images
    imageFromMeta

    -- * Misc stuff
  , loadImages
  , cloneImage
  , deleteImageClone
  , verifyContent
  , packImage
  , ensureImage
  , snapshotContainerImage
  , destroyImage

    -- * Interenal functions, should not be exported
  , imageBasePath
  , imageVolumePath

  ) where

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
import Scrz.Btrfs


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


-- | Create a new image given the meta description of it. The image id is
-- automatically created by hashing the meta record.
imageFromMeta :: ImageMeta -> Image
imageFromMeta meta = Image (mkImageId meta) (Just meta)


loadImages :: IO (Map String Image)
loadImages = do
    ids <- filter (\x -> '.' /= head x) <$> getDirectoryContents baseImageDirectory
    images <- mapM loadImageMeta ids
    return $ M.fromList $ zip ids images

  where

    loadImageMeta :: String -> IO Image
    loadImageMeta imgId = do
        meta <- readMetaFile imgId
        return $ maybe (Image imgId Nothing) (Image imgId . Just) meta


cloneImage :: Image -> String -> IO ()
cloneImage image path = do
    btrfsSubvolSnapshot (imageVolumePath image) path


deleteImageClone :: String -> IO ()
deleteImageClone path = do
    btrfsSubvolDelete path


snapshotContainerImage :: TVar Container -> String -> IO ()
snapshotContainerImage container image = do
    id' <- atomically $ containerId <$> readTVar container
    let rootfsPath = "/srv/scrz/containers/" ++ id' ++ "/rootfs"

    btrfsSubvolSnapshot rootfsPath volumePath'

  where

    imagePath   = "/srv/scrz/images/" ++ image
    volumePath' = imagePath ++ "/volume"


verifyContent :: Image -> a -> (String -> Int -> IO a) -> IO a
verifyContent image def action = maybe (return def) doVerifyContent (imageMeta image)
  where
    doVerifyContent meta = do
        (checksum, size) <- hashFile (imageContentPathS $ imageId image)

        let checksumOk = isCorrectChecksum checksum meta
        let sizeOk     = isCorrectSize size meta

        if checksumOk && sizeOk
            then return def
            else action checksum size

whenIO :: IO Bool -> IO () -> IO ()
whenIO conditionAction thenAction = do
    condition <- conditionAction
    when condition thenAction

unlessIO :: IO Bool -> IO () -> IO ()
unlessIO conditionAction thenAction = do
    condition <- conditionAction
    unless condition thenAction


ensureImage :: Image -> IO ()
ensureImage image = do

    -- Verify the content file if it already exists. If the verification
    -- fails, delete the file and we'll try to download it again.
    whenIO (doesFileExist $ imageContentPath image) $ do
        logger $ "Verifying existing image content file"
        verifyContent image () $ \_ _ -> do
            logger $ "Content file is corrupt, deleting"
            removeFile (imageContentPath image)


    -- Check again if the file exists. It may have been removed if the
    -- verification just above failed.
    unlessIO (doesFileExist $ imageContentPath image) $ do
        downloadImage image


    -- FIXME: Only verify if we've just downloaded the file.
    verifyContent image () $ \_ _ -> do
        logger $ "Downloaded image has invalid checksum"
        error "Image checksum mismatch"


    unlessIO (doesDirectoryExist $ imageVolumePath image) $ do
        unpackImage image


downloadImage :: Image -> IO ()
downloadImage image = maybe (return ()) doDownloadImage (imageMeta image)
  where
    doDownloadImage :: ImageMeta -> IO ()
    doDownloadImage meta = do
        logger $ "Downloading image " ++ imageId image ++ " from " ++ imageUrl meta
        createDirectoryIfMissing True $ imageBasePath image
        downloadBinary (imageUrl meta) $ imageContentPath image
        writeMetaFile (imageId image) meta


destroyImage :: String -> IO ()
destroyImage localImageId = do
    volumeExists <- doesDirectoryExist volumePath
    when volumeExists $ do
        btrfsSubvolDelete volumePath

    removeDirectoryRecursive $ imageBasePathS localImageId

  where

    volumePath = imageVolumePathS localImageId


unpackImage :: Image -> IO ()
unpackImage image = do
    btrfsSubvolCreate (imageVolumePath image)
    unpackTarball (imageContentPath image) (imageVolumePath image)


createTarball :: String -> String -> IO ()
createTarball tgz path = do
    fatal =<< exec "tar" [ "-czf", tgz, "-C", path, "." ]


unpackTarball :: String -> String -> IO ()
unpackTarball tgz path = do
    fatal =<< exec "tar" [ "-xf", tgz, "-C", path ]


writeMetaFile :: String -> ImageMeta -> IO ()
writeMetaFile localImageId image = do
    LB.writeFile (imageMetaPathS localImageId) $ encode image


readMetaFile :: String -> IO (Maybe ImageMeta)
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
    writeMetaFile localImageId $ ImageMeta localImageId hash size

  where

    contentPath = imageContentPathS localImageId

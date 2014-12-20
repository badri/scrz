{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Functions related to handling images.

module Scrz.Image
  (
    -- * Misc stuff
    loadImages
  , loadImageManifest
  , deleteImageClone
  , destroyImage

  , loadImageFuzzy
  , hashFileSHA512
  , createTarball

  , fetchImage

  ) where

import           Data.Aeson
import           Data.Monoid
import           Data.Maybe

import           Data.Map (Map)
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Base16 as BS16
import qualified Data.ByteString.Lazy as LB

import           Data.Text (Text)
import qualified Data.Text as T

import qualified Crypto.Hash.SHA512 as SHA512
import qualified Crypto.Hash.SHA3   as SHA3

import Data.AppContainer.Types (ImageManifest(..))

import System.Directory
import System.FilePath

import Control.Monad
import Control.Monad.Except
import Control.Applicative
import Control.Concurrent.STM

import Scrz.Types
import Scrz.Utils
import Scrz.Http
import Scrz.Log
import Scrz.Btrfs


baseImageDirectory :: String
baseImageDirectory = "/var/lib/scrz/images"

imageBasePathS :: String -> String
imageBasePathS = (</>) baseImageDirectory

imageContentPathS :: String -> String
imageContentPathS image = imageBasePathS image </> "content"

imageVolumePathS :: String -> String
imageVolumePathS image = imageBasePathS image </> "rootfs"

imageMetaPathS :: String -> String
imageMetaPathS image = imageBasePathS image </> "meta"


listImageIds :: Scrz [Text]
listImageIds = do
    entries <- scrzIO $ getDirectoryContents baseImageDirectory
    return $ map T.pack $ filter (\x -> '.' /= head x) entries

loadImages :: Scrz [(Text, ImageManifest)]
loadImages = do
    ids <- listImageIds
    images <- mapM (\iId -> loadImageManifest ("/var/lib/scrz/images/" <> iId <> "/manifest")) ids
    return $ zip ids images

loadImageManifest :: Text -> Scrz ImageManifest
loadImageManifest path = do
    bs <- scrzIO $ LB.readFile $ T.unpack path
    case eitherDecode bs of
        Left e -> throwError $ InternalError (T.pack e)
        Right im -> return im


loadImageFuzzy :: Text -> Scrz (Text, ImageManifest)
loadImageFuzzy prefix = do
    ids <- listImageIds

    case filter (\x -> T.isPrefixOf prefix x) ids of
        [iId] -> (,) <$> pure iId <*> loadImageManifest ("/var/lib/scrz/images/" <> iId <> "/manifest")
        _     -> throwError $ InternalError $ "Not a unique image id: " <> prefix


deleteImageClone :: String -> IO ()
deleteImageClone path = do
    btrfsSubvolDelete path


unlessIO :: IO Bool -> IO () -> IO ()
unlessIO conditionAction thenAction = do
    condition <- conditionAction
    unless condition thenAction



data UIMEntry = UIMEntry
    { rURL :: !Text
    , rObjectId :: !Text
    } deriving (Show)

instance ToJSON UIMEntry where
    toJSON UIMEntry{..} = object [ "url" .= rURL, "objectId" .= rObjectId ]

instance FromJSON UIMEntry where
    parseJSON (Object o) = UIMEntry
        <$> o .: "url"
        <*> o .: "objectId"
    parseJSON _ = fail "UIMEntry"


fetchImage :: Text -> Scrz (Text, ImageManifest)
fetchImage name = do
    -- TODO: App Container Image Discovery
    url <- return $ "https://" <> name <> ".aci"
    -- throwError $ UInternalError ""

    let tmp = "/var/lib/scrz/tmp"
    liftIO $ do
        createDirectoryIfMissing True tmp
        createDirectoryIfMissing True $ "/var/lib/scrz/uim"
        createDirectoryIfMissing True $ "/var/lib/scrz/objects"
        createDirectoryIfMissing True $ "/var/lib/scrz/images"

    -- Map the URL to the ObjectId of the Image.
    uime@UIMEntry{..} <- liftIO $ do
        let oh = hashSHA512 $ encode url
        exists <- doesFileExist $ "/var/lib/scrz/uim/sha512-" <> oh
        if exists
            then do
                (fromJust . decode) <$> LB.readFile ("/var/lib/scrz/uim/sha512-" <> oh)

            else do
                -- Download the image into a temporary file.
                tmpId <- newId
                putStrLn $ "Using temporary file " ++ tmpId
                downloadBinary
                    (T.unpack url)
                    (tmp ++ "/" ++ tmpId ++ ".gz")

                -- Unzip the file
                fatal =<< exec "gunzip" [ (tmp ++ "/" ++ tmpId ++ ".gz") ]

                -- Determine the image ID.
                iId <- imageIdFromFile (tmp ++ "/" ++ tmpId)
                print $ "ImageId: " <> iId

                -- Move the file into the object store.
                renameFile (tmp ++ "/" ++ tmpId) ("/var/lib/scrz/objects/" <> T.unpack iId)
                let ir  = UIMEntry url iId
                    irb = encode ir

                -- Write out the UIM entry.
                LB.writeFile ("/var/lib/scrz/uim/sha512-" <> oh) irb

                return ir

    liftIO $ print uime

    -- Unpack the image into a btrfs volume.
    res <- liftIO $ do
        de <- doesDirectoryExist $ "/var/lib/scrz/images/" <> T.unpack rObjectId
        unless de $ do
            createDirectoryIfMissing True $ "/var/lib/scrz/images/" <> T.unpack rObjectId
            btrfsSubvolCreate $ "/var/lib/scrz/images/" <> T.unpack rObjectId <> "/rootfs"
            unpackTarball
                ("/var/lib/scrz/objects/" <> T.unpack rObjectId)
                ("/var/lib/scrz/images/" <> T.unpack rObjectId)

        mf <- LB.readFile $ "/var/lib/scrz/images/" <> T.unpack rObjectId <> "/manifest"
        return $ eitherDecode mf

    case res of
        Left e -> throwError $ InternalError $ T.pack $ show e
        Right im -> return (rObjectId, im)

-- | Given a path to a (possible compressed) image file, return the image ID.
imageIdFromFile :: String -> IO Text
imageIdFromFile path = do
    (hash, _) <- hashFileSHA512 path
    return $ "sha512-" <> T.pack hash



destroyImage :: String -> IO ()
destroyImage localImageId = do
    volumeExists <- doesDirectoryExist vp
    when volumeExists $ do
        btrfsSubvolDelete vp

    removeDirectoryRecursive $ imageBasePathS localImageId

  where

    vp = imageVolumePathS localImageId


createTarball :: String -> String -> IO ()
createTarball tgz path = do
    fatal =<< exec "tar" [ "-cf", tgz, "-C", path, "." ]


unpackTarball :: String -> String -> IO ()
unpackTarball tgz path = do
    fatal =<< exec "tar" [ "-xf", tgz, "-C", path ]


hashSHA512 :: LB.ByteString -> String
hashSHA512 = BS8.unpack . BS16.encode . SHA512.hashlazy

hashFileSHA512 :: String -> IO (String, Int)
hashFileSHA512 path = do
    contents <- LB.readFile path
    let size = fromIntegral $ LB.length contents
    return $ (hashSHA512 contents, size)

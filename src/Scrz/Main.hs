{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}

module Main where


import Control.Monad.Except
import Control.Monad.Reader

import Options.Applicative
import Options.Applicative.Types

import System.Directory (renameFile, getDirectoryContents, copyFile)
import System.Posix.Files
import Network.Etcd
import Data.Maybe

import           Data.Time.Format.Human


import           Data.Monoid
import           Data.Time.Clock.POSIX

import           Data.Text (Text)
import qualified Data.Text as T

import           Data.AppContainer.Types

import           Data.Aeson
import           Data.UUID
import           Data.SemVer
import           Data.Text.Encoding
import qualified Data.ByteString.Lazy as LB

import           System.Random

import Scrz.Image
import Scrz.Types
import Scrz.Log
import Scrz.Host
import Scrz.Btrfs
import Scrz.Utils



-- | The top-level data type describing what the user wants to do with us.
data Invocation = Invocation
    { iOptions :: !Options
    , iCommand :: !Command
    }

-- | Global options which available to all commands.
data Options = Options
    { optEtcd :: !(Maybe Text)
    }

data Command
    = Version
    | Fetch !Text
    | Clone !Text !Text
    | ListImages
    | ShowWorkspace
    | RemoveWorkspaceImage !Text
    | PackImage !Text !Text
    | ListContainers !Text
    | CreateContainer !Text !Text
    | RemoveContainer !Text !Text


etcdClient :: Options -> Scrz Client
etcdClient opts = liftIO $ case optEtcd opts of
    Nothing -> createClient ["http://localhost:4001"]
    Just hn -> createClient [hn]



workspacePath :: Text
workspacePath = "/var/lib/scrz/workspace/"


main :: IO ()
main = do

    disableOutputBuffering

    run =<< execParser
        (parseInvocation `withInfo` "scrz")

run :: Invocation -> IO ()
run (Invocation opts Version) = do
    putStrLn "v0.0.1"

run (Invocation opts (Fetch name)) = do
    res <- runExceptT $ fetchImage name
    case res of
        Left e -> error $ show e
        Right (oid, _) -> putStrLn $ T.unpack oid

run (Invocation opts (Clone dst src)) = do
    ret <- runExceptT $ do
        (iId, im) <- loadImageFuzzy src
        mkdir workspacePath
        createVolumeSnapshot
            (workspacePath <> dst <> "/rootfs")
            ("/var/lib/scrz/images/" <> iId <> "/rootfs")

    case ret of
        Left e -> error $ show e
        Right _ -> return ()

run (Invocation opts ListImages) = do
    ires <- runExceptT loadImages
    let images = case ires of
            Left e -> error (show e)
            Right x -> x


    let headers = [ "NAME", "ID" ]
    rows <- mapM toRow $ images
    tabWriter $ headers : rows

  where

    toRow :: (Text, ImageManifest) -> IO [String]
    toRow (localImageId, ImageManifest{..}) = do
        return
            [ T.unpack imName
            , T.unpack $ T.take 27 localImageId
            ]

run (Invocation opts ShowWorkspace) = do
    ires <- runExceptT $ do
        entries <- scrzIO $ getDirectoryContents "/var/lib/scrz/workspace"
        return $ map T.pack $ filter (\x -> '.' /= head x) entries

    let volumes = case ires of
            Left e -> error (show e)
            Right x -> x


    let headers = [ "NAME", "CREATED AT" ]
    rows <- mapM toRow $ volumes
    tabWriter $ headers : rows

  where

    toRow :: Text -> IO [String]
    toRow volumeName = do
        fs <- getFileStatus $ "/var/lib/scrz/workspace/" <> T.unpack volumeName
        let mtime = statusChangeTime fs
        timeDiff <- humanReadableTime $ posixSecondsToUTCTime $ realToFrac mtime
        return
            [ T.unpack volumeName
            , timeDiff
            ]

run (Invocation opts (RemoveWorkspaceImage name)) = do
    ires <- runExceptT $ do
        scrzIO $ btrfsSubvolDelete $ "/var/lib/scrz/workspace/" <> T.unpack name <> "/rootfs"

    case ires of
        Left e -> error (show e)
        Right x -> return x

run (Invocation opts (PackImage manifest name)) = do
    ires <- runExceptT $ do
        void $ loadImageManifest manifest
        scrzIO $ copyFile (T.unpack manifest) (T.unpack $ workspacePath <> name <> "/manifest")
        tmpId <- scrzIO $ newId
        scrzIO $ createTarball
            ("/var/lib/scrz/tmp/" <> tmpId)
            (T.unpack $ workspacePath <> name)

        (hash, len) <- scrzIO $ hashFileSHA512 $ ("/var/lib/scrz/tmp/" <> tmpId)

        scrzIO $ renameFile
            ("/var/lib/scrz/tmp/" <> tmpId)
            ("/var/lib/scrz/objects/sha512-" <> hash)

        scrzIO $ putStrLn $ "sha512-" <> hash
        return ()

    case ires of
        Left e -> error (show e)
        Right _ -> return ()

run (Invocation opts (ListContainers mId)) = do
    ires <- runExceptT $ do
        client <- etcdClient opts
        containers <- liftIO $ listDirectoryContents client $
            "/scrz/hosts/" <> mId <> "/containers/"

        forM containers $ \node -> do
            mbNode <- liftIO $ get client $ (_nodeKey node <> "/manifest")
            case mbNode of
                Nothing -> throwError $ InternalError $ "No manifest"
                Just mfn -> case eitherDecode (rValueLB mfn) of
                    Left e -> throwError $ InternalError $ T.pack e
                    Right crm -> return crm

    case ires of
        Left e -> error (show e)
        Right containers -> do
            forM_ containers $ \ContainerRuntimeManifest{..} ->
                print crmUUID

run (Invocation opts (CreateContainer mId imageName)) = do
    ires <- runExceptT $ do
        client <- etcdClient opts
        uuid <- liftIO $ randomIO
        let crm = ContainerRuntimeManifest
                { crmUUID = uuid
                , crmVersion = version 0 1 1 [] []
                , crmImages = [ Image imageName "" ]
                , crmVolumes = []
                }

        void $ liftIO $ set client
            ("/scrz/hosts/" <> mId <> "/containers/" <> T.pack (Data.UUID.toString uuid) <> "/manifest")
            (decodeUtf8 $ LB.toStrict $ encode crm)
            Nothing

        return crm


    case ires of
        Left e -> error (show e)
        Right ContainerRuntimeManifest{..} -> do
            print crmUUID
            return ()


run (Invocation opts (RemoveContainer mId cId)) = do
    ires <- runExceptT $ do
        client <- etcdClient opts
        liftIO $ removeDirectoryRecursive client $
            "/scrz/hosts/" <> mId <> "/containers/" <> cId

    case ires of
        Left e -> error (show e)
        Right _ -> return ()


rValueLB :: Node -> LB.ByteString
rValueLB response = LB.fromStrict $ encodeUtf8 $ fromJust $ _nodeValue response

parseInvocation :: Parser Invocation
parseInvocation = Invocation <$> parseOptions <*> parseCommand

parseOptions :: Parser Options
parseOptions = Options
    <$> optional (option text (long "etcd-host" <> metavar "HOSTNAME"))

parseCommand :: Parser Command
parseCommand = subparser $ mconcat
    [ command "version"
        (parseVersion `withInfo` "Print the version and exit")

    , command "fetch"
        (parseFetch `withInfo` "Fetch an image")

    , command "clone"
        (parseClone `withInfo` "Clone an image to a temporary directory")

    , command "list-images"
        (parseListImages `withInfo` "List all images on the host")

    , command "show-workspace"
        (parseShowWorkspace `withInfo` "Show volumes in the workspace")

    , command "remove-workspace-image"
        (parseRemoveWorkspaceImage `withInfo` "Remove an image from the workspace")

    , command "pack-image"
        (parsePackImage `withInfo` "Pack a workspace image into an ACI archive")

    , command "list-containers"
        (parseListContainers `withInfo` "List all containers on a particular host")

    , command "create-container"
        (parseCreateContainer `withInfo` "Create a new container from an image name on the given host")

    , command "remove-container"
        (parseRemoveContainer `withInfo` "Remove a container from the etcd runtime configuration")
    ]


parseVersion :: Parser Command
parseVersion = pure Version

parseFetch :: Parser Command
parseFetch = Fetch
    <$> argument text (metavar "URL")

parseClone :: Parser Command
parseClone = Clone
    <$> argument text (metavar "DST")
    <*> argument text (metavar "SRC")

parseListImages :: Parser Command
parseListImages = pure ListImages

parseShowWorkspace :: Parser Command
parseShowWorkspace = pure ShowWorkspace

parseRemoveWorkspaceImage :: Parser Command
parseRemoveWorkspaceImage = RemoveWorkspaceImage
    <$> argument text (metavar "IMAGE-NAME")

parsePackImage :: Parser Command
parsePackImage = PackImage
    <$> argument text (metavar "MANIFEST")
    <*> argument text (metavar "IMAGE-NAME")

parseListContainers :: Parser Command
parseListContainers = ListContainers
    <$> argument text (metavar "MACHINE")

parseCreateContainer :: Parser Command
parseCreateContainer = CreateContainer
    <$> argument text (metavar "MACHINE")
    <*> argument text (metavar "IMAGE-NAME")

parseRemoveContainer :: Parser Command
parseRemoveContainer = RemoveContainer
    <$> argument text (metavar "MACHINE")
    <*> argument text (metavar "CONTAINER")

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc


text :: ReadM Text
text = ReadM (asks T.pack)



-- run :: [ String ] -> IO ()
--
-- run [ "pack-image", id' ]            = packImage id'
-- run [ "list-images" ]                = listImages
-- run [ "destroy-image", id' ]         = destroyImage id'
--
-- run [ "fetch", url, checksum, size ] = downloadImage url checksum (read size)
-- run [ "update-service-image", etcdHost, host, service, image, url] = updateServiceImage etcdHost host service image url
--
--
-- run [ "clone-image", localImageId, newImageId ] = do
--     let srcImage = Image localImageId Nothing
--     let dstImage = Image newImageId Nothing
--
--     createDirectoryIfMissing True (imageBasePath dstImage)
--     cloneImage srcImage (imageVolumePath dstImage)

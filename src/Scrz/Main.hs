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
import Data.SemVer
import qualified Data.ByteString.Lazy as LB
import Scrz.Etcd
import Scrz.Scrzfile
import qualified Data.Text.IO as T

import           Data.Time.Format.Human


import           Data.Monoid
import           Data.Time.Clock.POSIX

import           Data.Text (Text)
import qualified Data.Text as T

import           Data.Map (Map)
import qualified Data.Map as M

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
    | Build !Text
    | ListImages
    | ShowWorkspace
    | RemoveWorkspaceImage !Text
    | PackImage !Text !Text
    | ListMachines
    | ListContainers !MachineId
    | CreateContainer ![Text] !MachineId !Text
    | RemoveContainer !MachineId !ContainerId


etcdClient :: Options -> Scrz Client
etcdClient opts = scrzIO $ case optEtcd opts of
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

run (Invocation opts (Build scrzfile)) = do
    sf <- runExceptT $ do
        wsId <- T.pack <$> scrzIO newId
        let containerRootfs = (workspacePath <> wsId <> "/rootfs")

        contents <- scrzIO $ T.readFile $ T.unpack scrzfile
        ctx <- mkContext containerRootfs
        sf <- case parseScrzfile ctx contents of
            Left e -> fail e
            Right x -> pure x

        -- scrzIO $ print sf
        (oId, _) <- fetchImage (baseImage sf)

        -- scrzIO $ print wsId
        mkdir workspacePath

        -- scrzIO $ putStrLn $ "Creating subvolume from " <> show oId

        createVolumeSnapshot
            containerRootfs
            ("/var/lib/scrz/images/" <> oId <> "/rootfs")

        -- scrzIO $ putStrLn "Executing build instructions"
        forM (buildInstructions sf) $ \bi -> do
            scrzIO $ do
                -- putStrLn "Executing..."
                -- print bi
                return ()

            case bi of
                (Run (Cmd cmd args)) -> do
                    p <- scrzIO $ exec
                        (T.unpack cmd)
                        (map T.unpack args)

                    scrzIO $ fatal p

                (Spawn bindings (Cmd cmd ca)) -> do
                    let args = [ "-D", T.unpack containerRootfs
                               , "-j"
                               ] ++ map (\(Binding src path) -> "--bind=" <> T.unpack src <> ":" <> T.unpack path) bindings
                                ++ ["--"]
                                ++ [T.unpack cmd]
                                ++ map T.unpack ca

                    -- scrzIO $ print args
                    p <- scrzIO $ exec "systemd-nspawn" args
                    scrzIO $ fatal p



        let imageManifest = ImageManifest
                { imName = name sf
                , imVersion = version 0 1 1 [] []
                , imLabels = []
                , imApp = app sf
                , imDependencies = []
                }

        objId <- packImage imageManifest (workspacePath <> wsId)
        scrzIO $ T.putStrLn objId

        btrfsSubvolDelete
            (T.unpack $ workspacePath <> wsId <> "/rootfs")

    case sf of
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
        btrfsSubvolDelete $ "/var/lib/scrz/workspace/" <> T.unpack name <> "/rootfs"

    case ires of
        Left e -> error (show e)
        Right x -> return x

run (Invocation opts (PackImage manifest name)) = do
    ires <- runExceptT $ do
        im <- loadImageManifest manifest
        h <- packImage im (workspacePath <> name)
        scrzIO $ putStrLn $ T.unpack $ h

    case ires of
        Left e -> error (show e)
        Right _ -> return ()

run (Invocation opts ListMachines) = do
    ires <- runExceptT $ do
        client <- etcdClient opts
        listMachineIds client

    machineIds <- case ires of
        Left e -> error $ show e
        Right x -> return x

    let headers = [ "MACHINE ID", "HOSTNAME" ]
    rows <- mapM toRow machineIds
    tabWriter $ headers : rows

  where

    toRow :: MachineId -> IO [String]
    toRow mId = do
        md <- runExceptT $ do
            client <- etcdClient opts
            machineDescription client mId

        hostname <- case md of
            Left _ -> return "-"
            Right Nothing -> return "-"
            Right (Just Machine{..}) -> return machHostName

        return
            [ T.unpack $ unMachineId mId
            , T.unpack hostname
            ]


run (Invocation opts (ListContainers mId)) = do
    ires <- runExceptT $ do
        client <- etcdClient opts
        containerIds <- listContainerIds client mId

        forM containerIds $ \cId -> do
            mbCRM <- lookupContainerRuntimeManifest client mId cId
            case mbCRM of
                Nothing -> throwError $ InternalError $ "CRM not found"
                Just crm -> return crm

    containers <- case ires of
        Left e -> error $ show e
        Right x -> return x

    let headers = [ "CONTAINER ID", "APP" ]
    rows <- mapM toRow containers
    tabWriter $ headers : rows

  where

    toRow :: ContainerRuntimeManifest -> IO [String]
    toRow ContainerRuntimeManifest{..} = do
        let Image{..} = head crmImages
        return
            [ Data.UUID.toString crmUUID
            , T.unpack imageApp
            ]


run (Invocation opts (CreateContainer bindings mId imageName)) = do
    ires <- runExceptT $ do
        client <- etcdClient opts
        uuid <- scrzIO $ randomIO
        volumes <- forM bindings $ \binding -> do
            case T.splitOn ":" binding of
                source : fulfills : [] ->
                    return $ Volume [fulfills] $ HostVolumeSource $
                        HostVolume source False
                _ -> fail $ "Could not parse binding " <> T.unpack binding

        let crm = ContainerRuntimeManifest
                { crmUUID    = uuid
                , crmVersion = version 0 1 1 [] []
                , crmImages  = [ Image imageName "" ]
                , crmVolumes = volumes
                }

        void $ scrzIO $ set client
            (containerManifestKey mId (ContainerId uuid))
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
        scrzIO $ removeDirectoryRecursive client (containerKey mId cId)

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

    , command "build"
        (parseBuild `withInfo` "Build an image using instructions in a Scrzfile")

    , command "show-workspace"
        (parseShowWorkspace `withInfo` "Show volumes in the workspace")

    , command "remove-workspace-image"
        (parseRemoveWorkspaceImage `withInfo` "Remove an image from the workspace")

    , command "pack-image"
        (parsePackImage `withInfo` "Pack a workspace image into an ACI archive")

    , command "list-machines"
        (parseListMachines `withInfo` "List all machines")

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

parseBuild :: Parser Command
parseBuild = Build
    <$> argument text (metavar "SCRZFILE")

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

parseListMachines :: Parser Command
parseListMachines = pure ListMachines

parseListContainers :: Parser Command
parseListContainers = ListContainers
    <$> argument machineIdRead (metavar "MACHINE")

parseCreateContainer :: Parser Command
parseCreateContainer = CreateContainer
    <$> many (option text (long "bind" <> metavar "SOURCE:FULFILLS"))
    <*> argument machineIdRead (metavar "MACHINE")
    <*> argument text (metavar "IMAGE-NAME")

parseRemoveContainer :: Parser Command
parseRemoveContainer = RemoveContainer
    <$> argument machineIdRead (metavar "MACHINE")
    <*> argument containerIdRead (metavar "CONTAINER")

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc


text :: ReadM Text
text = ReadM (asks T.pack)

machineIdRead :: ReadM MachineId
machineIdRead = ReadM (asks (MachineId . T.strip . T.pack))

containerIdRead :: ReadM ContainerId
containerIdRead = ReadM $ do
    val <- ask
    case fromString val of
        Nothing -> fail "Not an UUID"
        Just v  -> return $ ContainerId v


packImage :: ImageManifest -> Text -> Scrz Text
packImage im ws = do
    tmpId <- scrzIO $ newId

    scrzIO $ LB.writeFile
        (T.unpack $ ws <> "manifest")
        (encode im)

    scrzIO $ createTarball
        ("/var/lib/scrz/tmp/" <> tmpId)
        (T.unpack ws)

    (hash, len) <- scrzIO $ hashFileSHA512 $ ("/var/lib/scrz/tmp/" <> tmpId)

    scrzIO $ renameFile
        ("/var/lib/scrz/tmp/" <> tmpId)
        ("/var/lib/scrz/objects/sha512-" <> hash)

    return $ "sha512-" <> T.pack hash



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

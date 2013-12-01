module Scrz.Protocol where

import Prelude hiding (id)
import qualified Data.Map as M
import Data.Maybe
import Data.List (intercalate)
import Data.Aeson
import Data.Aeson.Types
import Control.Applicative
import Control.Concurrent.STM
import System.IO
import Data.Time.Format.Human

import Scrz.Log
import Scrz.Types
import Scrz.Container
import Scrz.Image


data Command
  = Quit
  | CreateContainer Service
  | ListContainers
  | StopContainer String
  | Start String
  | DestroyContainer String
  | Snapshot String String
  | Run String [String] String [(String, String)]
  | Wait String


instance FromJSON Command where
    parseJSON (Object o) = do
        command <- o .: "command"
        parseCommand command

      where

        parseCommand :: String -> Parser Command
        parseCommand "quit" = return Quit
        parseCommand "list-container" = return ListContainers
        parseCommand "stop-container" = StopContainer <$> (o .: "id")
        parseCommand "create-container" = CreateContainer <$> (o .: "service")
        parseCommand "destroy-container" = DestroyContainer <$> (o .: "id")
        parseCommand "snapshot" = Snapshot <$> (o .: "container") <*> (o .: "image")
        parseCommand "start" = Start <$> (o .: "id")
        parseCommand "run" = Run <$> (o .: "image") <*> (o .: "cmd") <*> (o .: "pts") <*> (o .: "mounts")
        parseCommand "wait" = Wait <$> (o .: "id")
        parseCommand _ = fail "Command"

    parseJSON _ = fail "Command"

instance ToJSON Command where
    toJSON Quit =
        let command = "quit" :: String
        in object ["command" .= command]

    toJSON ListContainers =
        let command = "list-container" :: String
        in object ["command" .= command]

    toJSON (StopContainer id) =
        let command = "stop-container" :: String
        in object ["command" .= command, "id" .= id]

    toJSON (DestroyContainer id) =
        let command = "destroy-container" :: String
        in object ["command" .= command, "id" .= id]

    toJSON (CreateContainer service) =
        let command = "create-container" :: String
        in object ["command" .= command, "service" .= service]

    toJSON (Snapshot container image) =
        let command = "snapshot" :: String
        in object ["command" .= command, "container" .= container, "image" .= image]

    toJSON (Start id) =
        let command = "start" :: String
        in object ["command" .= command, "id" .= id]

    toJSON (Run image cmd pts mounts) =
        let command = "run" :: String
        in object ["command" .= command, "image" .= image, "cmd" .= cmd, "pts" .= pts, "mounts" .= mounts]

    toJSON (Wait id) =
        let command = "wait" :: String
        in object ["command" .= command, "id" .= id]


data Response
  = EmptyResponse
  | ErrorResponse
  | CreateContainerResponse String
  | ListContainersResponse [ Container ]
  deriving (Show)


instance FromJSON Response where
    parseJSON (Object o) = do
        response <- o .: "response"
        parseResponse response

      where

        parseResponse :: String -> Parser Response
        parseResponse "empty" = return EmptyResponse
        parseResponse "create-container" = CreateContainerResponse <$> (o .: "id")
        parseResponse "list-containers" = ListContainersResponse <$> (o .: "containers")
        parseResponse _ = fail "Response"

    parseJSON _ = fail "Response"

instance ToJSON Response where
    toJSON EmptyResponse =
        let response = "empty" :: String
        in object ["response" .= response]

    toJSON ErrorResponse =
        let response = "error" :: String
        in object ["response" .= response]

    toJSON (CreateContainerResponse id) =
        let response = "create-container" :: String
        in object ["response" .= response, "id" .= id]

    toJSON (ListContainersResponse d) =
        let response = "list-containers" :: String
        in object ["response" .= response, "containers" .= d]


processCommand :: TVar Runtime -> Command -> IO Response
processCommand _ Quit = do
    logger "Received <quit> command."
    error "exiting"

processCommand runtime (CreateContainer service) = do
    logger $ "Creating container " ++ (show $ serviceRevision service)

    container <- createContainer runtime Socket service (imageFromMeta $ serviceImage service)
    startContainer container Nothing
    id <- atomically $ containerId <$> readTVar container
    return $ CreateContainerResponse id

processCommand runtime ListContainers = do
    rt <- atomically $ readTVar runtime
    rows <- mapM (atomically . readTVar) $ M.elems (containers rt)
    return $ ListContainersResponse rows

processCommand runtime (StopContainer id) = do
    rt <- atomically $ readTVar runtime
    case M.lookup id (containers rt) of
        Nothing -> return EmptyResponse
        Just container -> do
            stopContainer container
            return EmptyResponse

processCommand runtime (DestroyContainer id) = do
    rt <- atomically $ readTVar runtime
    case M.lookup id (containers rt) of
        Nothing -> return EmptyResponse
        Just container -> do
            stopContainer container
            destroyContainer runtime container
            return EmptyResponse

processCommand runtime (Snapshot cid image) = do
    rt <- atomically $ readTVar runtime
    case M.lookup cid (containers rt) of
        Nothing -> return EmptyResponse
        Just container -> do
            stopContainer container
            snapshotContainerImage container image

            return EmptyResponse

processCommand runtime (Start id) = do
    rt <- atomically $ readTVar runtime
    case M.lookup id (containers rt) of
        Nothing -> return EmptyResponse
        Just container -> do
            startContainer container Nothing
            return EmptyResponse


processCommand runtime (Run url command pts mounts) = do
    handle <- openFile pts ReadWriteMode
    let meta = ImageMeta url "" 0
    let image = imageFromMeta meta

    let service = Service { serviceId = 0
      , serviceRevision = 0
      , serviceImage = meta
      , serviceCommand = command
      , serviceAddress = Nothing
      , serviceEnvironment = []
      , servicePorts = []
      , serviceVolumes = map (\(a,b) -> Volume a (Just b)) mounts
      }

    container <- createContainer runtime Socket service image
    startContainer container (Just handle)
    id <- atomically $ containerId <$> readTVar container
    return $ CreateContainerResponse id

processCommand runtime (Wait id) = do
    rt <- atomically $ readTVar runtime
    case M.lookup id (containers rt) of
        Nothing -> return EmptyResponse
        Just container -> do
            logger $ "Waiting until container " ++ id ++ " shuts down"
            atomically $ do
                ct <- readTVar container
                if isJust $ containerProcess ct
                    then retry
                    else return ()

            logger $ "Container " ++ id ++ " has shut down"
            return EmptyResponse


printResponse :: Response -> IO ()
printResponse EmptyResponse = do
    putStrLn "Empty response"

printResponse ErrorResponse = do
    putStrLn "Error response"

printResponse (CreateContainerResponse id) = do
    putStrLn $ "Created container " ++ id

printResponse (ListContainersResponse containers) = do
    let headers = [ "ID", "CREATED", "IMAGE", "COMMAND", "AUTHORITY", "ADDRESS", "STATUS" ]
    rows <- mapM listContainerItemRow containers
    tabWriter $ headers : rows

listContainerItemRow :: Container -> IO [ String ]
listContainerItemRow Container{..} = do
    timeDiff <- humanReadableTime containerCreatedAt
    return [ containerId
           , timeDiff
           , imageId containerImage
           , intercalate " " (serviceCommand containerService)
           , show containerAuthority
           , show containerAddress
           , if isJust containerProcess then "running" else "stopped"
           ]

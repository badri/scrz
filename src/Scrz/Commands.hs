module Scrz.Commands where

import qualified Data.Map as M
import Data.Maybe
import Data.List (intersperse, concat, transpose, intercalate)
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as L
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import qualified Data.Foldable as F

import Scrz.Log
import Scrz.Types
import Scrz.Container
import Scrz.Image


data Command
  = Quit
  | CreateContainer String String
  | ListContainers
  | StopContainer String
  | Start String
  | DestroyContainer String
  | Snapshot String String
  deriving (Show)


instance FromJSON Command where
    parseJSON (Object o) = do
        command <- o .: "command"
        parseCommand command o

      where

        parseCommand :: String -> Object -> Parser Command
        parseCommand "quit" o = return Quit
        parseCommand "list-container" o = return ListContainers
        parseCommand "stop-container" o = StopContainer <$> (o .: "id")
        parseCommand "create-container" o = CreateContainer <$> (o .: "image") <*> (o .: "service")
        parseCommand "destroy-container" o = DestroyContainer <$> (o .: "id")
        parseCommand "snapshot" o = Snapshot <$> (o .: "container") <*> (o .: "image")
        parseCommand "start" o = Start <$> (o .: "id")
        parseCommand _ _ = fail "Command"


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

    toJSON (CreateContainer image service) =
        let command = "create-container" :: String
        in object ["command" .= command, "image" .= image, "service" .= service]

    toJSON (Snapshot container image) =
        let command = "snapshot" :: String
        in object ["command" .= command, "container" .= container, "image" .= image]

    toJSON (Start id) =
        let command = "start" :: String
        in object ["command" .= command, "id" .= id]

data Response
  = EmptyResponse
  | CreateContainerResponse String
  | ListContainersResponse [ [ String ] ]
  deriving (Show)

instance FromJSON Response where
    parseJSON (Object o) = do
        response <- o .: "response"
        parseResponse o response

      where

        parseResponse :: Object -> String -> Parser Response
        parseResponse o "empty" = return EmptyResponse
        parseResponse o "create-container" = CreateContainerResponse <$> (o .: "id")
        parseResponse o "list-containers" = ListContainersResponse <$> (o .: "data")

instance ToJSON Response where
    toJSON EmptyResponse =
        let response = "empty" :: String
        in object ["response" .= response]

    toJSON (CreateContainerResponse id) =
        let response = "create-container" :: String
        in object ["response" .= response, "id" .= id]

    toJSON (ListContainersResponse d) =
        let response = "list-containers" :: String
        in object ["response" .= response, "data" .= d]


processCommand :: TVar Runtime -> Command -> IO Response
processCommand runtime Quit = do
    logger "Received <quit> command."
    error "exiting"

processCommand runtime (CreateContainer image service) = do
    logger $ "Creating container " ++ image ++ " " ++ service

    let img = Image image
    rt <- atomically $ readTVar runtime
    case M.lookup service (services rt) of
        Nothing -> do
            logger "service not found"
            return EmptyResponse
        Just s -> do
            container <- createContainer runtime img s
            startContainer runtime container
            id <- atomically $ containerId <$> readTVar container
            return $ CreateContainerResponse id

processCommand runtime ListContainers = do
    rt <- atomically $ readTVar runtime
    rows <- mapM dumpContainer $ M.elems (containers rt)
    return $ ListContainersResponse rows

  where

    dumpContainer :: TVar Container -> IO [ String ]
    dumpContainer container = do
        c <- atomically $ readTVar container

        let cid = containerId c
        let iid = imageId $ containerImage c
        let cmd = head $ serviceCommand $ containerService c
        let sta = if isJust $ containerProcess c then "running" else "stopped"
        return [ cid, iid, cmd, sta ]

processCommand runtime (StopContainer id) = do
    rt <- atomically $ readTVar runtime
    case M.lookup id (containers rt) of
        Nothing -> return EmptyResponse
        Just container -> do
            stopContainer runtime container
            return EmptyResponse

processCommand runtime (DestroyContainer id) = do
    rt <- atomically $ readTVar runtime
    case M.lookup id (containers rt) of
        Nothing -> return EmptyResponse
        Just container -> do
            stopContainer runtime container
            destroyContainer runtime container
            return EmptyResponse

processCommand runtime (Snapshot cid image) = do
    rt <- atomically $ readTVar runtime
    case M.lookup cid (containers rt) of
        Nothing -> return EmptyResponse
        Just container -> do
            stopContainer runtime container
            snapshotContainerImage container image

            return EmptyResponse

processCommand runtime (Start id) = do
    rt <- atomically $ readTVar runtime
    case M.lookup id (containers rt) of
        Nothing -> return EmptyResponse
        Just container -> do
            startContainer runtime container
            return EmptyResponse


printResponse :: Response -> IO ()
printResponse EmptyResponse = do
    putStrLn "Empty response"

printResponse (CreateContainerResponse id) = do
    putStrLn $ "Created container " ++ id

printResponse (ListContainersResponse rows) = do
    let headers = [ "ID", "IMAGE", "COMMAND", "STATUS" ]
    tabWriter $ headers : rows


tabWriter :: [ [ String ] ] -> IO ()
tabWriter d = do
    let lengths = map maximum $ transpose $ (map (map length) d)
    mapM_ (writeRow lengths) d

  where

    writeRow :: [ Int ] -> [ String ] -> IO ()
    writeRow lengths row = do
        let fields = map expandField (zip lengths row)
        putStrLn $ intercalate "   " fields

    expandField :: (Int, String) -> String
    expandField (maxLength, field) = fld ++ trail
        where fld   = take maxLength field
              trail = (replicate (maxLength - (length fld)) ' ')

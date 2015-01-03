{-# LANGUAGE OverloadedStrings #-}

module Scrz.Etcd
    ( listMachineIds

    , listContainerIds
    , lookupContainerRuntimeManifest

    , machineContainersKey
    , containerKey
    , containerManifestKey
    ) where

import Data.Maybe
import Data.Aeson
import qualified Data.ByteString.Lazy as LB
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Monoid
import Data.UUID
import Control.Monad.Except
import Network.Etcd
import Scrz.Types
import Data.AppContainer.Types


machineContainersKey :: MachineId -> Text
machineContainersKey mId = mconcat
    [ "/scrz/hosts/"
    , unMachineId mId
    , "/containers"
    ]

containerKey :: MachineId -> ContainerId -> Text
containerKey mId cId = mconcat
    [ "/scrz/hosts/"
    , unMachineId mId
    , "/containers/"
    , T.pack (toString (unContainerId cId))
    ]

containerManifestKey :: MachineId -> ContainerId -> Text
containerManifestKey mId cId = mconcat
    [ "/scrz/hosts/"
    , unMachineId mId
    , "/containers/"
    , T.pack (toString (unContainerId cId))
    , "/manifest"
    ]

listMachineIds :: Client -> Scrz [MachineId]
listMachineIds client = do
    let dir = "/scrz/hosts"
    keys <- scrzIO $ listDirectoryContents client dir
    return $ map (MachineId . T.drop (1 + T.length dir) . _nodeKey) keys


listContainerIds :: Client -> MachineId -> Scrz [ContainerId]
listContainerIds client mId = do
    let dir = machineContainersKey mId
    keys <- scrzIO $ listDirectoryContents client dir
    return $ catMaybes $ map (fmap ContainerId . fromString . T.unpack . T.drop (1 + T.length dir) . _nodeKey) keys


lookupContainerRuntimeManifest :: Client -> MachineId -> ContainerId -> Scrz (Maybe ContainerRuntimeManifest)
lookupContainerRuntimeManifest client mId cId = do
    mbNode <- scrzIO $ get client (containerManifestKey mId cId)
    case mbNode of
        Nothing -> return Nothing
        Just node -> case eitherDecode (rValueLB node) of
            Left e -> throwError $ InternalError $ T.pack e
            Right x -> return x

rValueLB :: Node -> LB.ByteString
rValueLB response = LB.fromStrict $ encodeUtf8 $ fromJust $ _nodeValue response

-- /v1/keys/scrz/hosts/{machine-id}/containers/{uuid}/manifest

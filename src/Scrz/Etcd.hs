{-# LANGUAGE OverloadedStrings #-}

module Scrz.Etcd
    ( listContainerIds
    , lookupContainerRuntimeManifest

    , machineContainersKey
    , containerKey
    , containerManifestKey
    ) where

import Data.Maybe
import Data.Aeson
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as LB
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Monoid
import Data.UUID
import Control.Applicative
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Except
import Network.Etcd
import Scrz.Utils
import Scrz.Types
import Scrz.Host
import Data.AppContainer.Types


localEtcdServer :: [Text]
localEtcdServer = ["http://localhost:4001"]

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

listContainerIds :: Client -> MachineId -> Scrz [Text]
listContainerIds client mId = do
    let dir = machineContainersKey mId
    keys <- scrzIO $ listDirectoryContents client dir
    return $ map (T.drop (T.length dir) . _nodeKey) keys


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

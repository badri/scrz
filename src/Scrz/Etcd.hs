{-# LANGUAGE OverloadedStrings #-}

module Scrz.Etcd
    ( listContainerIds
    ) where

import Data.Maybe
import Data.Aeson
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as LB
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Monoid
import Control.Applicative
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Except
import Network.Etcd
import Scrz.Utils
import Scrz.Types
import Scrz.Host

localEtcdServer :: [Text]
localEtcdServer = ["http://localhost:4001"]

listContainerIds :: IO [Text]
listContainerIds = do
    Right mId <- runExceptT $ machineId
    parseServices mId

  where
    parseServices mId = do
        client <- createClient localEtcdServer
        let dir = "/scrz/hosts/" <> mId <> "/containers/"
        keys <- listDirectoryContents client dir
        return $ map (T.drop (T.length dir) . _nodeKey) keys

-- /v1/keys/scrz/hosts/{machine-id}/containers/{uuid}/manifest

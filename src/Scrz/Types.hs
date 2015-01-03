{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Scrz.Types where


import           Data.Aeson
import           Data.Aeson.TH
import           Language.Haskell.TH
import           Data.Char


import           Data.UUID

import           Data.Text (Text)
import qualified Data.Text as T

import           Control.Applicative
import           Control.Monad.Except
import           Control.Exception.Base

import           Scrz.TH



type Scrz = ExceptT Error IO

data Error
    = InternalError !Text
    deriving (Show)


newtype MachineId = MachineId { unMachineId :: Text }
    deriving (Eq, Show)

instance ToJSON MachineId where
    toJSON (MachineId x) = toJSON x

instance FromJSON MachineId where
    parseJSON (String x) = pure $ MachineId x
    parseJSON _          = fail "MachineId"


data Machine = Machine
    { machId :: !MachineId
    , machHostName :: !Text
    } deriving (Eq, Show)


newtype ContainerId = ContainerId { unContainerId :: UUID }
    deriving (Eq, Show)

instance ToJSON ContainerId where
    toJSON (ContainerId uuid) = toJSON $ toString uuid

instance FromJSON ContainerId where
    parseJSON x = do
        str <- parseJSON x
        case fromString str of
            Nothing -> fail "ContainerId"
            Just uuid -> pure $ ContainerId uuid



scrzIO :: IO a -> Scrz a
scrzIO m = do
    res <- liftIO $ (Right <$> m) `catch` handleException

    case res of
        Left e -> throwError e
        Right r -> return r

  where
    handleException :: SomeException -> IO (Either Error a)
    handleException e = case asyncExceptionFromException e of
        Nothing -> return $ Left $ InternalError $ T.pack $ show e
        Just ae -> throwIO (ae :: SomeAsyncException)


$(deriveMyJSON "mach" ''Machine)

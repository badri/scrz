{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Scrz.Types where

import           GHC.Generics (Generic)

import           Data.Aeson
import           Data.Bits
import           Data.Char (chr)
import           Data.Hashable
import           Data.List as L
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (isJust)
import           Data.Set (Set)
import           Data.Time.Clock
import           Data.Word

import           Data.Text (Text)
import qualified Data.Text as T

import           Text.Printf

import           Control.Applicative
import           Control.Monad.Except
import           Control.Concurrent.STM
import           Control.Exception.Base

import           System.Posix.Types

import           Scrz.Aeson


type Scrz = ExceptT Error IO

data Error
    = InternalError !Text
    deriving (Show)


newtype MachineId = MachineId { unMachineId :: Text }


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

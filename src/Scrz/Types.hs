{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Scrz.Types where


import           Data.UUID

import           Data.Text (Text)
import qualified Data.Text as T

import           Control.Applicative
import           Control.Monad.Except
import           Control.Exception.Base



type Scrz = ExceptT Error IO

data Error
    = InternalError !Text
    deriving (Show)


newtype MachineId = MachineId { unMachineId :: Text }


newtype ContainerId = ContainerId { unContainerId :: UUID }


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

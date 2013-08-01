module Scrz.Http (getJSON, patchJSON, downloadBinary) where

import           Data.Aeson
import           Data.Conduit
import           Data.Conduit.Binary (sinkFile)

import           Control.Applicative
import           Control.Monad

import           Network.HTTP.Conduit
import           Network.HTTP.Types.Method


getJSON :: (FromJSON a) => String -> IO (Maybe a)
getJSON url = do
    req <- acceptJSON <$> parseUrl url
    body <- responseBody <$> (withManager $ httpLbs req)
    return $ decode body

  where

    acceptJSON req = req { requestHeaders = acceptHeader : requestHeaders req }
    acceptHeader = ("Accept","application/json")


patchJSON :: (ToJSON a) => String -> a -> IO ()
patchJSON url body = do
    req' <- parseUrl url
    let req = req'
            { method = "PATCH"
            , requestBody = RequestBodyLBS (encode body)
            , requestHeaders = ("Content-Type","application/json") : requestHeaders req'
            }

    void $ withManager $ httpLbs req


downloadBinary :: String -> String -> IO ()
downloadBinary url localPath = do
    req <- parseUrl url
    let req' = req { method = methodGet }
    withManager $ \manager -> do
        response <- http req' manager
        responseBody response $$+- sinkFile localPath

module Scrz.Http (getJSON, patchJSON, downloadBinary, postUrlEncoded) where

import           Data.Aeson
import           Data.ByteString.Char8 (pack)
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

postUrlEncoded :: String -> [(String, String)] -> IO ()
postUrlEncoded url params = do
    req' <- parseUrl url
    let req = urlEncodedBody (map (\(k,v) -> (pack k, pack v)) params) $ req'

    void $ withManager $ httpLbs req

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

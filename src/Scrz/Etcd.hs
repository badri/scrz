module Scrz.Etcd (listServices) where

import Data.Maybe
import Data.Aeson
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LC8
import Control.Applicative
import Control.Monad
import Scrz.Http
import Scrz.Utils
import Scrz.Types
import Scrz.Commands ()

withMaybe :: Maybe a -> (a -> IO ()) -> IO ()
withMaybe Nothing  _ = return ()
withMaybe (Just a) f = f a

baseUrl :: String
baseUrl = "http://localhost:4001/v1"

-- | The response that etcd can return to clients. Inside the etcd sources,
-- the response is represented with a single type (store/store.go).
data Response = Response
  { rAction    :: String
  , rKey       :: String
  , rDir       :: Maybe Bool
    -- ^ Present and true if the key has no value itself, but has subkeys.
    --   I've never seen it present and false.

  , rPrevValue :: Maybe String
  , rValue     :: Maybe String
  , rNewKey    :: Maybe Bool
  , rTTL       :: Maybe Int
  , rIndex     :: Int
  } deriving (Show)

instance FromJSON Response where
    parseJSON (Object o) = Response
        <$> o .:  "action"
        <*> o .:  "key"
        <*> o .:? "dir"
        <*> o .:? "prevValue"
        <*> o .:? "value"
        <*> o .:? "newKey"
        <*> o .:? "ttl"
        <*> o .:  "index"

    parseJSON _ = fail "Etcd/Response"

rValueLB :: Response -> LB.ByteString
rValueLB response = LC8.pack $ fromJust $ rValue response

listKeys :: String -> IO [String]
listKeys path = do
    reply' <- getJSON $ baseUrl ++ "/keys" ++ path
    case reply' :: Maybe [Response] of
        Nothing -> return []
        Just reply -> return $ map rKey reply

getKey :: String -> IO (Maybe Response)
getKey path = do
    getJSON $ baseUrl ++ "/keys" ++ path

putKey :: String -> String -> IO ()
putKey path value = do
    postUrlEncoded (baseUrl ++ "/keys" ++ path) [("value", value)]


listServices :: IO [Service]
listServices = do
    fqdn <- fullyQualifiedDomainName
    maybe (return []) parseServices fqdn

  where
    parseServices fqdn = do
        keys <- listKeys $ "/hosts/" ++ fqdn ++ "/services"
        services <- catMaybes <$> mapM getKey keys
        print services

        return $ map (fromJust . decode . rValueLB) services


-- /v1/keys/hosts/azeroth.caurea.org/services/rstgj5432fstd
-- /v1/keys/hosts/azeroth.caurea.org/runtime

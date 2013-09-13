module Scrz.Etcd (listServices, updateRuntime) where

import Data.Maybe
import Data.Aeson
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LC8
import Control.Applicative
import Control.Exception
import Control.Concurrent.STM
import Scrz.Http
import Scrz.Utils
import Scrz.Types
import Scrz.Commands ()

baseUrl :: String
baseUrl = "http://localhost:4001/v1"

-- | The response that etcd can return to clients. Inside the etcd sources,
-- the response is represented with a single type (store/store.go).
data Response = Response
  { rKey       :: String
  , rValue     :: Maybe String
  } deriving (Show)

instance FromJSON Response where
    parseJSON (Object o) = Response
        <$> o .:  "key"
        <*> o .:? "value"

    parseJSON _ = fail "Etcd/Response"

rValueLB :: Response -> LB.ByteString
rValueLB response = LC8.pack $ fromJust $ rValue response

listKeys :: String -> IO [String]
listKeys path = (flip catch) (\(_ :: SomeException) -> return []) $ do
    reply' <- getJSON $ baseUrl ++ "/keys" ++ path
    case reply' :: Maybe [Response] of
        Nothing -> return []
        Just reply -> return $ map rKey reply

getKey :: String -> IO (Maybe Response)
getKey path = (flip catch) (\(_ :: SomeException) -> return Nothing) $ do
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

        return $ map (fromJust . decode . rValueLB) services

updateRuntime :: Runtime -> IO ()
updateRuntime runtime = do
    conts <- mapM (atomically . readTVar . snd) (M.toList $ containers runtime)
    fqdn <- fullyQualifiedDomainName
    maybe (return ()) (setRuntimeValue conts) fqdn

  where
    setRuntimeValue conts fqdn = do
        let path  = "/hosts/" ++ fqdn ++ "/runtime"
        let value = LC8.unpack $ encode conts

        res <- getKey path
        case res of
            Nothing -> putKey path value
            Just x -> do
                if rValue x == Just value
                    then return ()
                    else putKey path value


-- /v1/keys/hosts/azeroth.caurea.org/services/rstgj5432fstd
-- /v1/keys/hosts/azeroth.caurea.org/runtime

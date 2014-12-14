module Scrz.Etcd
    ( listServices
    , updateRuntime
    , updateServiceImage
    ) where

import Data.Maybe
import Data.Aeson
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LC8
import Control.Applicative
import Control.Concurrent.STM
import Control.Monad
import Network.Etcd
import Scrz.Utils
import Scrz.Image
import Scrz.Types
import Scrz.Protocol ()

localEtcdServer :: [String]
localEtcdServer = ["http://localhost:4001"]

rValueLB :: Node -> LB.ByteString
rValueLB response = LC8.pack $ fromJust $ _nodeValue response

listServices :: IO [Service]
listServices = do
    fqdn <- fullyQualifiedDomainName
    maybe (return []) parseServices fqdn

  where
    parseServices fqdn = do
        client <- createClient localEtcdServer
        keys <- listDirectoryContents client $ "/hosts/" ++ fqdn ++ "/services"
        services <- catMaybes <$> mapM (get client . _nodeKey) keys

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

        client <- createClient localEtcdServer
        res <- get client path
        case res of
            Nothing -> void $ set client path value Nothing
            Just x -> do
                if _nodeValue x == Just value
                    then return ()
                    else void $ set client path value Nothing


updateServiceImage :: String -> String -> String -> String -> String -> IO ()
updateServiceImage etcdHost host service imgId url = do
    client <- createClient [etcdHost]

    let key = "/hosts/" ++ host ++ "/services/" ++ service
    putStrLn $ "Service key: " ++ key

    res <- get client key
    case res of
        Nothing -> do
            putStrLn "Service not found"
        Just node -> do
            print node
            case decode $ rValueLB node of
                Nothing -> do
                    putStrLn "Could not decode node value"
                Just svc -> do
                    img <- loadImageMeta imgId
                    case imageMeta img of
                        Nothing -> do
                            putStrLn "Image meta is not available"
                        Just meta -> do
                            let meta' = meta { imageUrl = url }
                            let svc' = svc { serviceImage = meta' }
                            print svc'
                            void $ set client key (LC8.unpack $ encode svc') Nothing


-- /v1/keys/hosts/host/services/rstgj5432fstd
-- /v1/keys/hosts/host/runtime

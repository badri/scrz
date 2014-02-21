module Scrz.Etcd (listServices, updateRuntime) where

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


-- /v1/keys/hosts/azeroth.caurea.org/services/rstgj5432fstd
-- /v1/keys/hosts/azeroth.caurea.org/runtime

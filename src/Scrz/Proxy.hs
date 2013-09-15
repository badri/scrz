module Scrz.Proxy where

import qualified Data.List as L
import Data.Aeson
import Control.Applicative
import Control.Exception
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad

import Scrz.Log
import Scrz.Http
import Scrz.Utils


data ProxyRuntime = ProxyRuntime
  { serviceAddress :: String
  , proxy :: Proxy
  } deriving (Eq, Show)

data Proxy = Proxy
  { proxyServices :: [ ProxyService ]
  } deriving (Eq, Show)

instance FromJSON Proxy where
    parseJSON (Object o) = Proxy
        <$> o .: "services"

    parseJSON _ = fail "Proxy"


data ProxyService = ProxyService
  { proxyServicePort :: Int
  , proxyServiceTargets :: [ ProxyTarget ]
  } deriving (Eq, Show)

instance FromJSON ProxyService where
    parseJSON (Object o) = ProxyService
        <$> o .: "port"
        <*> o .: "targets"

    parseJSON _ = fail "ProxyService"


data ProxyTarget = ProxyTarget
  { proxyTargetHost :: String
  , proxyTargetPort :: Int
  } deriving (Eq, Show)

instance FromJSON ProxyTarget where
    parseJSON (Object o) = ProxyTarget
        <$> o .: "host"
        <*> o .: "port"

    parseJSON _ = fail "ProxyTarget"


mkProxyRuntime :: String -> IO (TVar ProxyRuntime)
mkProxyRuntime addr = newTVarIO $ ProxyRuntime addr $ Proxy []


insertProxyService :: TVar ProxyRuntime -> ProxyService -> IO ()
insertProxyService runtime proxyService = do
    rt <- atomically $ readTVar runtime
    let saddr = (serviceAddress rt) ++ ":" ++ (show $ proxyServicePort proxyService)
    fatal =<< exec "ipvsadm" [ "-A", "-t", saddr ]
    forM_ (proxyServiceTargets proxyService) $ \target -> do
        let raddr = (proxyTargetHost target) ++ ":" ++ (show $ proxyTargetPort target)
        fatal =<< exec "ipvsadm" [ "-a", "-t", saddr, "-r", raddr, "-m" ]

    atomically $ modifyTVar runtime $ \x ->
        x { proxy = Proxy $ proxyService : (proxyServices $ proxy x) }

removeProxyService :: TVar ProxyRuntime -> ProxyService -> IO ()
removeProxyService runtime proxyService = do
    rt <- atomically $ readTVar runtime
    wait =<< exec "ipvsadm" [ "-D", "-t", (serviceAddress rt) ++ ":" ++ (show $ proxyServicePort proxyService) ]

    atomically $ modifyTVar runtime $ \x ->
        x { proxy = Proxy $ L.filter (/= proxyService) (proxyServices $ proxy x) }


mergeProxyConfig :: TVar ProxyRuntime -> Proxy -> IO ()
mergeProxyConfig runtime config = do
    rt <- atomically $ readTVar runtime
    forM_ (proxyServices config) $ \pp -> do
        let hasPort = L.elem pp (proxyServices $ proxy rt)

        when (not hasPort) $ do
            removeProxyService runtime pp
            logger $ "Port removed from proxy config"

    forM_ (proxyServices config) addService

  where

    addService :: ProxyService -> IO ()
    addService pp = do
        rt <- atomically $ readTVar runtime
        let exists = L.elem pp (proxyServices $ proxy rt)

        unless exists $ do
            insertProxyService runtime pp
            logger $ show pp


ipvsProxy :: String -> String -> IO ()
ipvsProxy addr url = do
    runtime <- mkProxyRuntime addr

    forever $ do
        putStrLn "arst"
        syncProxyConfig runtime `catch` \(e :: SomeException) -> do
            logger $ "Syncing proxy config with remote authority failed: " ++ show e

        threadDelay $ 10 * 1000 * 1000

  where

    syncProxyConfig runtime = do
        fqdn' <- fullyQualifiedDomainName
        withMaybe fqdn' $ \fqdn -> do
            config' <- getJSON $ url ++ "/api/hosts/" ++ fqdn ++ "/proxy-config"
            print config'
            withMaybe config' $ \config -> do
                mergeProxyConfig runtime config

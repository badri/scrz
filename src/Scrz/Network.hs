module Scrz.Network
  (

    initializeNetwork
  , cleanupNetwork

  , allocateAddress
  , releaseAddress

  , allocatePorts
  , releasePorts

  ) where


import           Data.Set (Set)
import qualified Data.Set as S

import Control.Monad
import Control.Concurrent.STM

import System.Process

import Scrz.Log
import Scrz.Types
import Scrz.Utils
import Scrz.Network.IPv4


------------------------------------------------------------------------------
-- Exported symbols


initializeNetwork :: IO (IPv4, Set IPv4, Set Int)
initializeNetwork = do

    cleanupNetwork

    logger "Initializing iptables"

    iptables fatal [ "-t", "nat", "-N", "SCRZ" ]
    iptables fatal [ "-t", "nat", "-A", "OUTPUT", "-j", "SCRZ" ]
    iptables fatal [ "-t", "nat", "-A", "PREROUTING", "-m", "addrtype", "--dst-type", "LOCAL", "-j", "SCRZ" ]

    return $ ( toIPv4 [10,1,0,1], addresses,  ports )

  where

    list      = take 100 $ iterate (1+) 2
    addresses = S.fromList $ map (\x -> toIPv4 [10,1,0,x]) list
    ports     = S.fromDistinctAscList [ 50000 .. 59999 ]


cleanupNetwork :: IO ()
cleanupNetwork = do
    logger "Cleaning up iptables configuration"

    iptables wait [ "-t", "nat", "-D", "PREROUTING", "-m", "addrtype", "--dst-type", "LOCAL", "-j", "SCRZ" ]
    iptables wait [ "-t", "nat", "-D", "OUTPUT", "-j", "SCRZ" ]

    iptables wait [ "-t", "nat", "-F", "SCRZ" ]
    iptables wait [ "-t", "nat", "-X", "SCRZ" ]



allocateAddress :: TVar Runtime -> IO IPv4
allocateAddress runtime = atomically $ do
    rt@Runtime{..} <- readTVar runtime
    let addr = head $ S.toList networkAddresses
    writeTVar runtime $ rt { networkAddresses = S.delete addr networkAddresses }
    return addr


releaseAddress :: TVar Runtime -> IPv4 -> IO ()
releaseAddress runtime addr = atomically $ do
    modifyTVar runtime $ \x -> x { networkAddresses = S.insert addr (networkAddresses x)}



allocatePorts :: TVar Runtime -> IPv4 -> [ Port ] -> IO [ Int ]
allocatePorts runtime addr ports = do
    externalPorts <- forM ports (allocatePort runtime)
    mapPorts addr (zip externalPorts ports)
    return externalPorts


releasePorts :: TVar Runtime -> IPv4 -> [ Port ] -> [ Int ] -> IO ()
releasePorts runtime addr internalPorts externalPorts = do
    unmapPorts addr (zip externalPorts internalPorts)
    mapM_ (releasePort runtime) externalPorts


-----------------------------------------------------------------------------
-- Private symbols

allocatePort :: TVar Runtime -> Port -> IO Int
allocatePort runtime Port{..} = atomically $ do
    maybe randomFromSet returnIfAvailable externalPort

  where

    -- Allocation a random port from the set. It's not actually random but the
    -- next one in the set.
    randomFromSet = do
        rt@Runtime{..} <- readTVar runtime

        -- FIXME: We may have run out of network ports. 'head' here may throw
        -- an exception.
        let ext = head $ S.toList networkPorts
        writeTVar runtime $ rt { networkPorts = S.delete ext networkPorts }
        return ext

    -- Check if the given port is available. throw an exception if not.
    returnIfAvailable ext = do
        Runtime{..} <- readTVar runtime
        unless (S.member ext networkPorts) $ do
            error $ "Exteral port " ++ (show ext) ++ " already allocated"

        return ext


releasePort :: TVar Runtime -> Int -> IO ()
releasePort runtime port = atomically $ do
    modifyTVar runtime $ \x -> x { networkPorts = S.insert port (networkPorts x)}


mapPorts :: IPv4 -> [ (Int,Port) ] -> IO ()
mapPorts addr ports = do
    mapM_ (\x -> updateForwardRule "-A" addr (fst x) (internalPort $ snd x)) ports


unmapPorts :: IPv4 -> [ (Int,Port) ] -> IO ()
unmapPorts addr ports = do
    mapM_ (\x -> updateForwardRule "-D" addr (fst x) (internalPort $ snd x)) ports


updateForwardRule :: String -> IPv4 -> Int -> Int -> IO ()
updateForwardRule rule addr src dst = iptables wait
    [ "-t", "nat", rule, "SCRZ", "-p", "tcp", "--dport", show src
    , "-j", "DNAT", "--to-destination", (show addr) ++ ":" ++ (show dst)
    ]


iptables :: (ProcessHandle -> IO ()) -> [String] -> IO ()
iptables f args = void $ exec "iptables" args >>= f

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


------------------------------------------------------------------------------
-- Exported symbols


initializeNetwork :: IO (IPv4, Set IPv4, Set Int)
initializeNetwork = do

    cleanupNetwork

    logger "Initializing network"

 -- Create bridge interface
    fatal =<< exec "ip" [ "link", "add", "scrz", "type", "bridge" ]
    fatal =<< exec "ip" [ "addr", "add", show scrzIfaceAddress ++ "/24", "dev", "scrz" ]
    fatal =<< exec "ip" [ "link", "set", "scrz", "up" ]

 -- Setup iptable rules
    iptables fatal [ "-t", "nat", "-N", "SCRZ" ]
    iptables fatal [ "-t", "nat", "-A", "OUTPUT", "-j", "SCRZ" ]
    iptables fatal [ "-t", "nat", "-A", "PREROUTING", "-m", "addrtype", "--dst-type", "LOCAL", "-j", "SCRZ" ]
    iptables fatal [ "-t", "nat", "-A", "POSTROUTING", "-s", addr, "!", "-d", addr, "-j", "MASQUERADE" ]

 -- Enable IPv4 forwarding in the kernel
    writeFile "/proc/sys/net/ipv4/ip_forward" "1"

    return $ ( scrzIfaceAddress, addresses,  ports )

  where

    addr      = show scrzIfaceAddress
    list      = take 100 $ iterate (1+) 2
    addresses = S.fromList $ map (\x -> toIPv4 [10,1,0,x]) list
    ports     = S.fromDistinctAscList [ 50000 .. 59999 ]


cleanupNetwork :: IO ()
cleanupNetwork = do
    logger "Cleaning up iptables configuration"

 -- Clean up iptables rules
    iptables wait [ "-t", "nat", "-D", "POSTROUTING", "-s", addr, "!", "-d", addr, "-j", "MASQUERADE" ]
    iptables wait [ "-t", "nat", "-D", "PREROUTING", "-m", "addrtype", "--dst-type", "LOCAL", "-j", "SCRZ" ]
    iptables wait [ "-t", "nat", "-D", "OUTPUT", "-j", "SCRZ" ]

    iptables wait [ "-t", "nat", "-F", "SCRZ" ]
    iptables wait [ "-t", "nat", "-X", "SCRZ" ]

 -- Destroy bridge interface
    wait =<< exec "ip" [ "link", "del", "scrz" ]

  where

    addr = show scrzIfaceAddress


allocateAddress :: TVar Runtime -> Maybe IPv4 -> IO IPv4
allocateAddress runtime desiredAddress = atomically $ do
    rt@Runtime{..} <- readTVar runtime
    let addr = maybe (head $ S.toList networkAddresses) id desiredAddress
    writeTVar runtime $ rt { networkAddresses = S.delete addr networkAddresses }
    return addr


releaseAddress :: TVar Runtime -> IPv4 -> IO ()
releaseAddress runtime addr = atomically $ do
    modifyTVar runtime $ \x -> x { networkAddresses = S.insert addr (networkAddresses x)}



allocatePorts :: TVar Runtime -> IPv4 -> [ Port ] -> IO [ Int ]
allocatePorts runtime addr ports = forM ports $ \port -> do
    externalPort <- allocatePort runtime port
    updateForwardRule "-A" addr externalPort (internalPort port)
    return externalPort


releasePorts :: TVar Runtime -> IPv4 -> [ Port ] -> [ Int ] -> IO ()
releasePorts runtime addr internalPorts externalPorts =
    forM_ (zip internalPorts externalPorts) $ \(Port{..}, ext) -> do
        updateForwardRule "-D" addr ext internalPort
        releasePort runtime ext



-----------------------------------------------------------------------------
-- Private symbols

-- | The bridge address is currently fixed, non-configurable.
scrzIfaceAddress :: IPv4
scrzIfaceAddress = toIPv4 [10,1,0,1]


allocatePort :: TVar Runtime -> Port -> IO Int
allocatePort runtime Port{..} = maybe randomFromSet return externalPort
  where

    -- Allocation a random port from the set. It's not actually random but the
    -- next one in the set.
    randomFromSet = atomically $ do
        rt@Runtime{..} <- readTVar runtime

        -- FIXME: We may have run out of network ports. 'head' here may throw
        -- an exception.
        let ext = head $ S.toList networkPorts
        writeTVar runtime $ rt { networkPorts = S.delete ext networkPorts }
        return ext


releasePort :: TVar Runtime -> Int -> IO ()
releasePort runtime port = atomically $ do
    modifyTVar runtime $ \x -> x { networkPorts = S.insert port (networkPorts x)}


updateForwardRule :: String -> IPv4 -> Int -> Int -> IO ()
updateForwardRule rule addr src dst = iptables wait
    [ "-t", "nat", rule, "SCRZ", "-p", "tcp", "--dport", show src
    , "-j", "DNAT", "--to-destination", (show addr) ++ ":" ++ (show dst)
    ]


iptables :: (ProcessHandle -> IO ()) -> [String] -> IO ()
iptables f args = void $ exec "iptables" args >>= f

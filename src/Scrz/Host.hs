module Scrz.Host
    ( machineId
    , mkdir
    , createVolumeSnapshot
    , disableOutputBuffering

    , btrfsSubvolCreate
    , btrfsSubvolSnapshot
    , btrfsSubvolDelete

    , fullyQualifiedDomainName
    ) where


import           Control.Applicative
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Concurrent

import           System.Directory

import           Data.UUID
import           Data.Monoid

import           Data.Text (Text)
import qualified Data.Text as T

import qualified Data.Map as M

import           Data.AppContainer.Types

import           System.Directory
import           System.FilePath
import           System.IO

import           Network.BSD (getHostName)
import           Network.Socket

import           Scrz.Types
import           Scrz.Utils



machineId :: Scrz MachineId
machineId = scrzIO $
    fmap (MachineId . T.strip . T.pack) $ readFile "/etc/machine-id"


mkdir :: Text -> Scrz ()
mkdir path = scrzIO $
    createDirectoryIfMissing True (T.unpack path)


createVolumeSnapshot :: Text -> Text -> Scrz ()
createVolumeSnapshot dst src =
    btrfsSubvolSnapshot (T.unpack src) (T.unpack dst)


disableOutputBuffering :: IO ()
disableOutputBuffering = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering



btrfsSubvolCreate :: String -> Scrz ()
btrfsSubvolCreate path = do
    createParentDirectory path
    scrzIO $ do
        fatal =<< exec "btrfs" [ "subvolume", "create", path ]


btrfsSubvolSnapshot :: String -> String -> Scrz ()
btrfsSubvolSnapshot src dst = do
    createParentDirectory dst
    scrzIO $ do
        fatal =<< exec "btrfs" [ "subvolume", "snapshot", src, dst ]


btrfsSubvolDelete :: String -> Scrz ()
btrfsSubvolDelete path = scrzIO $ do
    fatal =<< exec "btrfs" [ "subvolume", "delete", path ]


createParentDirectory :: String -> Scrz ()
createParentDirectory path = scrzIO $ do
    createDirectoryIfMissing True (parentDirectoryOf path)


parentDirectoryOf :: String -> String
parentDirectoryOf = joinPath . init . splitDirectories


fullyQualifiedDomainName :: Scrz (Maybe Text)
fullyQualifiedDomainName = scrzIO $ do
    hostName <- Just <$> getHostName
    addrInfo <- head <$> getAddrInfo Nothing hostName Nothing
    (fqdn, _) <- getNameInfo [] True False (addrAddress addrInfo)
    return $ fmap T.pack fqdn

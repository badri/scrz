
-- | Wrapper around btrfs commandline utility. The functions here will
-- automatically create parent directories if they are missing. If the btrfs
-- command exits with a non-zero exit status, it is treated as failure and the
-- functions thrown an exception.

module Scrz.Btrfs
  (

    -- * Subvolumes
    btrfsSubvolCreate
  , btrfsSubvolSnapshot
  , btrfsSubvolDelete

  ) where



import System.Directory
import System.FilePath

import Scrz.Utils



btrfsSubvolCreate :: String -> IO ()
btrfsSubvolCreate path = do
    createParentDirectory path
    fatal =<< exec "btrfs" [ "subvolume", "create", path ]


btrfsSubvolSnapshot :: String -> String -> IO ()
btrfsSubvolSnapshot src dst = do
    createParentDirectory dst
    fatal =<< exec "btrfs" [ "subvolume", "snapshot", src, dst ]


btrfsSubvolDelete :: String -> IO ()
btrfsSubvolDelete path = do
    fatal =<< exec "btrfs" [ "subvolume", "delete", path ]


createParentDirectory :: String -> IO ()
createParentDirectory path = do
    createDirectoryIfMissing True (parentDirectoryOf path)


parentDirectoryOf :: String -> String
parentDirectoryOf = joinPath . init . splitDirectories

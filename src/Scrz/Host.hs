module Scrz.Host
    ( machineId
    , mkdir
    , createVolumeSnapshot
    ) where


import           Data.Text (Text)
import qualified Data.Text as T

import           System.Directory

import           Scrz.Types
import           Scrz.Btrfs



machineId :: Scrz Text
machineId = scrzIO $
    fmap (T.strip . T.pack) $ readFile "/etc/machine-id"


mkdir :: Text -> Scrz ()
mkdir path = scrzIO $
    createDirectoryIfMissing True (T.unpack path)


createVolumeSnapshot :: Text -> Text -> Scrz ()
createVolumeSnapshot dst src = scrzIO $
    btrfsSubvolSnapshot (T.unpack src) (T.unpack dst)

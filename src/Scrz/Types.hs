{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Scrz.Types where

import           GHC.Generics (Generic)

import           Data.Aeson
import           Data.Bits
import           Data.Char (chr)
import           Data.Hashable
import           Data.List as L
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (isJust)
import           Data.Set (Set)
import           Data.Time.Clock
import           Data.Word

import           Text.Printf

import           Control.Applicative
import           Control.Concurrent.STM

import           System.Posix.Types

import           Scrz.Aeson


-- IPv4 {{{
data IPv4 = IPv4 Word32 deriving (Eq, Ord)

-- FIXME: This looks really bad in JSON. We should pretty-print the address.
$(deriveScrzJSON "" ''IPv4)

instance Show IPv4 where
    show (IPv4 a) = show4 a
      where
        remQuo x = (x `mod` 256, x `div` 256)
        show4 q = printf "%d.%d.%d.%d" a1 a2 a3 a4
          where
            (a4,q4) = remQuo q
            (a3,q3) = remQuo q4
            (a2,q2) = remQuo q3
            (a1, _) = remQuo q2


toIPv4 :: [ Int ] -> IPv4
toIPv4 [a1,a2,a3,a4] = IPv4 $ fromIntegral $ shift a1 24 + shift a2 16 + shift a3 8 + a4
toIPv4 _             = error "toIPv4: wrong number of octets"

-- FIXME: This is an orphan instance, apparently it's bad because GHC has
-- a warning which we have to explicitly disable.
$(deriveScrzJSON "" ''CPid)
-- }}}
-- ImageMeta {{{
data ImageMeta = ImageMeta
  { imageUrl :: String
  , imageChecksum :: String
  , imageSize :: Int
  } deriving (Show, Eq, Generic)

instance Hashable ImageMeta
$(deriveScrzJSON "image" ''ImageMeta)


isCorrectChecksum :: String -> ImageMeta -> Bool
isCorrectChecksum checksum image =
    if imageChecksum image == ""
        then True
        else checksum == imageChecksum image

isCorrectSize :: Int -> ImageMeta -> Bool
isCorrectSize size image =
    if imageSize image == 0
        then True
        else size == imageSize image
-- }}}
-- Image {{{
-- | An image that can be used to start a container. It has a local id, which
-- is used in the directory name under which the image is stored. The id must
-- be unique amongst all images on a particular host. If the image was created
-- from metadata (see 'imageFromMeta') then the id is automatically generated
-- by hashing the ImageMeta record.
data Image = Image
  { imageId :: String
  , imageMeta :: Maybe ImageMeta
  } deriving (Show, Eq)

$(deriveScrzJSON "image" ''Image)


hashChar :: Int -> Char
hashChar x
    | x < 26 = chr ((x     ) + 65)
    | x < 52 = chr ((x - 26) + 97)
    | x < 62 = chr ((x - 52) + 48)
    | otherwise = '-'

hashString :: Int -> String
hashString input
    | input < 0 = []
    | otherwise = hashChar b : hashString (a * b - 1)

  where

    (a, b) = divMod input 62

mkImageId :: ImageMeta -> String
mkImageId image = take 13 . hashString . abs . hash $ image
-- }}}
-- Port {{{

data Port = Port
  { internalPort :: Int
  -- ^ The container port that is to be exposed to the external network.

  , externalPort :: Maybe Int
  -- ^ If set, the container port is mapped to this specific external port.
  --   Since external ports have to be unique, only a single container can map
  --   to a specific port.
  } deriving (Show, Eq)

instance FromJSON Port where
    parseJSON (Object o) = Port
        <$> o .: "internal"
        <*> o .: "external"

    parseJSON _ = fail "Port"

instance ToJSON Port where
    toJSON Port{..} = object
        [ "internal"  .= internalPort
        , "external"  .= externalPort
        ]
-- }}}
-- Volume {{{
data Volume = Volume
  { volumePath :: String
  -- ^ Path inside the container to mount the volume to.

  , volumeBacking :: Maybe String
  -- ^ ID of the backing volume if one should be reused. If Nothing, then the
  --   supervisor creates a new backing volume.
  } deriving (Show, Eq)

$(deriveScrzJSON "volume" ''Volume)
-- }}}
-- Service {{{
data Service = Service
  { serviceId :: Int
  , serviceRevision :: Int
  , serviceImage :: ImageMeta
  , serviceCommand :: [ String ]
    -- ^ Command and arguments that are executed to start this service.

  , serviceEnvironment :: [ (String,String) ]

  , serviceAddress :: Maybe IPv4
  -- ^ If set, the container will use this address.

  , servicePorts :: [ Port ]
    -- ^ Network ports that the service requires. When a container starts,
    --   a mapping is created for these ports so that they are exposed to the
    --   external network.

  , serviceVolumes :: [ Volume ]
  } deriving (Show, Eq)

$(deriveScrzJSON "service" ''Service)
-- }}}
-- Config {{{
data Config = Config
  { configServices :: [ Service ]
  } deriving (Show, Eq)

$(deriveScrzJSON "config" ''Config)
-- }}}
-- BackingVolume {{{
data BackingVolume = AdHocVolume String | ManagedVolume
  { backingVolumeId :: String
  } deriving (Show)

$(deriveScrzJSON "backingVolume" ''BackingVolume)

backingVolumePath :: BackingVolume -> String
backingVolumePath (AdHocVolume path) = path
backingVolumePath (ManagedVolume vid) = "/srv/scrz/volumes/" ++ vid
-- }}}
-- Authority {{{
data Authority = Local | Socket | Remote String
    deriving (Eq, Show)

$(deriveScrzJSON "" ''Authority)
-- }}}
-- Container {{{
data Container = Container
  { containerId :: String

  , containerCreatedAt :: UTCTime

  , containerAuthority :: Authority
  , containerService :: Service
    -- ^ The service description as received from the authority server.

  , containerImage :: Image

  , containerAddress :: IPv4
  , containerPorts :: [ Int ]
    -- ^ External ports mapped for the service. Each port in the service has
    --   a corresponding port here.

  , containerVolumes :: [ BackingVolume ]

  , containerProcess :: Maybe ProcessID
    -- ^ The process (lxc-start) which runs the container.
  } deriving (Show)

implementsService :: Authority -> Service -> Container -> Bool
implementsService authority service Container{..} =
    containerAuthority == authority && containerService == service

$(deriveScrzJSON "container" ''Container)
-- }}}
-- Runtime {{{
data Runtime = Runtime
  { bridgeAddress :: IPv4
  -- ^ Bridge to which all containers are connected to.

  , networkAddresses :: Set IPv4
  -- ^ Unallocated network addresses in the same subnet as the bridge.

  , networkPorts :: Set Int
  -- ^ Unallocated external ports.

  , backingVolumes :: Map String BackingVolume
  , containers :: Map String (TVar Container)
  }


-- | Return true if the runtime has a container running that implements the
--   service.
hasContainer :: Runtime -> Authority -> Service -> IO Bool
hasContainer runtime authority service = do
    cs <- mapM (atomically . readTVar) (M.elems $ containers runtime)
    return $ isJust $ L.find (implementsService authority service) cs
-- }}}

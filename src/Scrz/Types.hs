module Scrz.Types where

import           GHC.Generics (Generic)

import           Data.Aeson
import           Data.Char (chr)
import           Data.Hashable
import           Data.List as L
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (isJust)
import           Data.Set (Set)
import           Data.Word


import           Control.Applicative
import           Control.Concurrent.STM
import           System.Posix.Types


-- | An image that can be used to start a container. It has a local id, which
-- is used in the directory name under which the image is stored. The id must
-- be unique amongst all images on a particular host. If the image was created
-- from metadata (see 'imageFromMeta') then the id is automatically generated
-- by hashing the ImageMeta record.
data Image = Image
  { imageId :: String
  , imageMeta :: Maybe ImageMeta
  } deriving (Show, Eq)

data ImageMeta = ImageMeta
  { imageUrl :: String
  , imageChecksum :: String
  , imageSize :: Int
  } deriving (Show, Eq, Generic)

instance Hashable ImageMeta

instance FromJSON ImageMeta where
    parseJSON (Object o) = ImageMeta
        <$> o .: "url"
        <*> o .: "checksum"
        <*> o .: "size"

    parseJSON _ = fail "Image"

instance ToJSON ImageMeta where
    toJSON image = object
        [ "url"      .= imageUrl image
        , "checksum" .= imageChecksum image
        , "size"     .= imageSize image
        ]


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


data Volume = Volume
  { volumePath :: String
  -- ^ Path inside the container to mount the volume to.

  , volumeBacking :: Maybe String
  -- ^ ID of the backing volume if one should be reused. If Nothing, then the
  --   supervisor creates a new backing volume.
  } deriving (Show, Eq)

instance FromJSON Volume where
    parseJSON (Object o) = Volume
        <$> o .: "path"
        <*> o .: "backing"

    parseJSON _ = fail "Volume"

instance ToJSON Volume where
    toJSON = error "ToJSON Volume"


data Service = Service
  { serviceId :: Int
  , serviceRevision :: Int
  , serviceImage :: ImageMeta
  , serviceCommand :: [ String ]
    -- ^ Command and arguments that are executed to start this service.

  , serviceEnvironment :: [ (String,String) ]

  , servicePorts :: [ Port ]
    -- ^ Network ports that the service requires. When a container starts,
    --   a mapping is created for these ports so that they are exposed to the
    --   external network.

  , serviceVolumes :: [ Volume ]
  } deriving (Show, Eq)

instance FromJSON Service where
    parseJSON (Object o) = Service
        <$> o .: "id"
        <*> o .: "revision"
        <*> o .: "image"
        <*> o .: "command"
        <*> o .: "environment"
        <*> o .: "ports"
        <*> o .: "volumes"

    parseJSON _ = fail "Service"

instance ToJSON Service where
    toJSON Service{..} = object
        [ "id"        .= serviceId
        , "revision"  .= serviceRevision
        ]


data Config = Config
  { configServices :: [ Service ]
  } deriving (Show, Eq)

instance FromJSON Config where
    parseJSON (Object o) = Config
        <$> o .: "services"

    parseJSON _ = fail "Config"


data BackingVolume = AdHocVolume String | ManagedVolume
  { backingVolumeId :: String
  }

backingVolumePath :: BackingVolume -> String
backingVolumePath (AdHocVolume path) = path
backingVolumePath (ManagedVolume vid) = "/srv/scrz/volumes/" ++ vid


data Authority = Local | Socket | Remote String
    deriving (Eq)

data Container = Container
  { containerId :: String

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
  }

implementsService :: Authority -> Service -> Container -> Bool
implementsService authority service Container{..} =
    containerAuthority == authority && containerService == service

instance ToJSON Container where
    toJSON Container{..} = object
        [ "id"        .= containerId
        , "service"   .= containerService
        , "port-map"  .= servicePorts containerService
        ]


data IPv4 = IPv4 Word32 deriving (Eq, Ord)

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

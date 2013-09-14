module Scrz.Volume
  (

    allocateVolumes
  , releaseVolumes

  ) where

import qualified Data.Map as M

import Control.Monad
import Control.Concurrent.STM

import Scrz.Types
import Scrz.Utils
import Scrz.Btrfs



------------------------------------------------------------------------------
-- Exported symbols


-- | Allocate all backing volumes for the given service. This function may
-- throw an exception.
allocateVolumes :: TVar Runtime -> Service -> IO [ BackingVolume ]
allocateVolumes runtime service = do
    forM (serviceVolumes service) (allocateVolume runtime)


-- Release all backing volumes.
releaseVolumes :: TVar Runtime -> [ BackingVolume ] -> IO ()
releaseVolumes runtime bv = forM_ bv (releaseVolume runtime)



------------------------------------------------------------------------------
-- Private symbols


baseVolumeDirectory :: String
baseVolumeDirectory = "/srv/scrz/volumes"


-- | Allocate a single backing volume. If no specific backing volume was
-- specified, a new btrfs subvolume will be created. If the backing volume
-- starts with a "/", then that is treated as an absolute path to an existing
-- btrfs subvolume. Otherwise it is treated as the ID of an existing, managed
-- backing volume.
allocateVolume :: TVar Runtime -> Volume -> IO BackingVolume
allocateVolume runtime volume = do
    case (volumeBacking volume) of
        Nothing -> createBackingVolume runtime
        Just x -> do
            if '/' == head x
                then return $ AdHocVolume x
                else do
                    rt <- atomically $ readTVar runtime
                    case M.lookup x (backingVolumes rt) of
                        Nothing -> error $ "Backing volume not available: " ++ x
                        Just b -> return b


-- | FIXME: Must not release the backing volume if it's a managed one and used
-- by another container.
releaseVolume :: TVar Runtime -> BackingVolume -> IO ()
releaseVolume _       (AdHocVolume _) = return ()
releaseVolume runtime (ManagedVolume id') = do
    atomically $ modifyTVar runtime $ \x ->
        x { backingVolumes = M.delete id' (backingVolumes x) }

    btrfsSubvolDelete (baseVolumeDirectory ++ "/" ++ id')


createBackingVolume :: TVar Runtime -> IO BackingVolume
createBackingVolume runtime = do
    id' <- newId
    let ret = ManagedVolume id'

    btrfsSubvolCreate (baseVolumeDirectory ++ "/" ++ id')
    atomically $ modifyTVar runtime $ \x ->
        x { backingVolumes = M.insert id' ret (backingVolumes x) }

    return ret

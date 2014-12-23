{-# LANGUAGE ForeignFunctionInterface #-}

module Scrz.Utils where

import Control.Monad.Random

import System.Process
import System.IO
import System.Exit
import Network.BSD (getHostName)
import Network.Socket
import Control.Applicative
import Control.Monad
import System.Posix.Types
import System.Posix.Process
import System.Posix.IO
import System.IO
import Data.Maybe
import Foreign.C.Types
import Control.Exception


newId :: IO String
newId = evalRandIO (sequence (replicate 10 rnd))
  where
    rnd = getRandomR ('a','z')


exec :: String -> [ String ] -> IO ProcessHandle
exec cmd args = do
    withFile "/dev/null" WriteMode $ \fh -> do
        (_, _, _, p) <- createProcess ((proc cmd args) { std_out = UseHandle fh, std_err = UseHandle fh })
        return p

execEnv :: String -> [ String ] -> [ (String,String) ] -> IO ProcessHandle
execEnv cmd args environment = do
    (_, _, _, p) <- createProcess $ (proc cmd args) { env = Just environment }
    return p

wait :: ProcessHandle -> IO ()
wait = void . waitForProcess

fatal :: ProcessHandle -> IO ()
fatal p = do
    exitCode <- waitForProcess p
    case exitCode of
        ExitSuccess -> return ()
        _ -> error $ "Exited with " ++ (show exitCode)

kill :: ProcessHandle -> IO ()
kill = terminateProcess

fullyQualifiedDomainName :: IO (Maybe String)
fullyQualifiedDomainName = do
    hostName <- Just <$> getHostName
    addrInfo <- head <$> getAddrInfo Nothing hostName Nothing
    fst <$> getNameInfo [] True False (addrAddress addrInfo)

withMaybe :: Maybe a -> (a -> IO ()) -> IO ()
withMaybe Nothing  _ = return ()
withMaybe (Just a) f = f a

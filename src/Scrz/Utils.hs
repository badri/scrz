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
import Data.Maybe
import Foreign.C.Types
import Control.Exception

foreign import ccall unsafe "login_tty"
  c_login_tty :: CInt -> IO CInt

newSession :: Fd -> IO ()
newSession fd = do
    void $ c_login_tty (fromIntegral fd)

newId :: IO String
newId = evalRandIO (sequence (replicate 10 rnd))
  where
    rnd = getRandomR ('a','z')


exec :: String -> [ String ] -> IO ProcessHandle
exec cmd args = do
    (_, _, _, p) <- createProcess (proc cmd args)
    return p

execEnv :: String -> [ String ] -> [ (String,String) ] -> Maybe Handle -> IO ProcessID
execEnv cmd args environment mbHandle = do
    child <- forkProcess $ do
        when (isJust mbHandle) $ do
            fd <- handleToFd $ fromJust mbHandle
            newSession fd

        executeFile cmd True args (Just environment)

    return child

waitE :: ProcessID -> IO ()
waitE p = void (getProcessStatus True True p) `catch` ignoreException
  where
    ignoreException :: SomeException -> IO ()
    ignoreException _ = return ()

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

{-# LANGUAGE ForeignFunctionInterface #-}

module Scrz.Utils
    ( kill
    , exec
    , fatal
    , newId
    , wait
    ) where

import Control.Monad.Random

import System.Process
import System.IO
import System.Exit
import Control.Monad


newId :: IO String
newId = evalRandIO (sequence (replicate 10 rnd))
  where
    rnd = getRandomR ('a','z')


exec :: String -> [ String ] -> IO ProcessHandle
exec cmd args = do
    withFile "/dev/null" WriteMode $ \fh -> do
        (_, _, _, p) <- createProcess ((proc cmd args) { std_out = UseHandle fh, std_err = UseHandle fh })
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

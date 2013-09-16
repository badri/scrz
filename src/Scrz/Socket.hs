module Scrz.Socket where

import Data.Aeson

import Control.Monad

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString.Lazy
import Prelude hiding (getContents)

import Control.Concurrent.STM
import Control.Exception

import Scrz.Log
import Scrz.Types
import Scrz.Protocol


controlSocketPath :: String
controlSocketPath = "/var/run/scrz.sock"

controlSocketAddress :: SockAddr
controlSocketAddress = SockAddrUnix controlSocketPath

createControlSocket :: IO Socket
createControlSocket = socket AF_UNIX Stream 0

serverSocket :: IO Socket
serverSocket = do
    sock <- createControlSocket
    bind sock controlSocketAddress
    listen sock 10

    return sock


handleClient :: TVar Runtime -> Socket -> IO ()
handleClient runtime sock = do
    (clientSock, _) <- accept sock

    bytes <- recv clientSock 99999
    case decode bytes of
        Nothing -> do
            logger "Could not decode command"
            close clientSock
            return ()

        Just cmd -> do
            response <- processCommand runtime cmd
            sendAll clientSock $ encode response
            close clientSock

clientSocket :: IO Socket
clientSocket = do
    sock <- createControlSocket
    connect sock controlSocketAddress

    return sock

sendCommand :: Command -> IO Response
sendCommand command = do
    sock <- clientSocket

    sendCommand_ sock `catch` \(e :: SomeException) -> do
        logger $ "Got exception: " ++ show e
        return ErrorResponse

  where

    sendCommand_ sock = do
        sendAll sock (encode command)
        response <- recv sock 999999
        close sock
        case decode' response of
            Nothing -> error $ "Unable to parse response: " ++ show response
            Just resp -> return resp

module Main where

import           Control.Monad                  ( forever )
                                            -- `Stream` is data constructor of `SocketType`
                                            -- `AI_PASSIVE` is data constructor of `AddrInfoFlag`
import           Network.Socket                 ( AddrInfoFlag(AI_PASSIVE)
                                                , Socket
                                                , SocketType(Stream)
                                                , accept
                                                , addrAddress
                                                , addrFamily
                                                , addrFlags
                                                , bind
                                                , close
                                                , defaultHints
                                                , defaultProtocol
                                                , getAddrInfo
                                                , listen
                                                , socket
                                                , withSocketsDo
                                                )
import           Network.Socket.ByteString      ( recv
                                                , sendAll
                                                )

-- | Print the literal representation of text sent
logAndEcho :: Socket -> IO ()
logAndEcho sock = forever $ do
  -- listen for new client connections
  let printAndKickback conn = do
        -- `recv` action receive up to 1024 bytes of text from the client
        msg <- recv conn 1024
        print msg
        sendAll conn msg
  -- `accept` action block until client connects to the server
  (soc, _) <- accept sock
  printAndKickback soc
  -- close the communication session, not the `sock`
  close soc

main :: IO ()
-- `withSocketsDo` is not going to do anything unless the program is ran on Windows.
-- On Windows, `withSocketsDo` makes it obligatory to use the sockets API in the `network` library.
main = withSocketsDo $ do
  addrInfos <- getAddrInfo (Just (defaultHints { addrFlags = [AI_PASSIVE] })) -- :: Maybe AddrInfo 
                           Nothing     -- :: Maybe Hostname
                           (Just "79") -- listen for connections on port "79"
  let serverAddr = head addrInfos
  -- `socket` construct a socket descriptor
  sock <- socket (addrFamily serverAddr) Stream defaultProtocol
  -- `bind` binds the socket descriptor with `sock`
  bind sock (addrAddress serverAddr)
  -- let the OS know that this program is prepared to listen for 
  -- connections from client with `listen`
  listen sock 1
  -- trigger the logic to run indefinitely as defined above
  logAndEcho sock
  -- when done ▼
  close sock

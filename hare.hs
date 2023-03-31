{-# OPTIONS_GHC -Wno-deprecations #-}

import Control.Monad
import Data.ByteString.Char8 (hPutStrLn, pack, unpack)
import GHC.IO.Handle
  ( BufferMode (NoBuffering),
    hClose,
    hGetLine,
    hSetBuffering,
  )
import GHC.IO.IOMode (IOMode (ReadWriteMode))
import Network.Socket
  ( AddrInfo (addrAddress, addrFamily, addrSocketType),
    PortNumber,
    SocketOption (ReuseAddr),
    SocketType (Stream),
    accept,
    bind,
    defaultHints,
    defaultProtocol,
    getAddrInfo,
    listen,
    setSocketOption,
    socket,
    socketToHandle,
    withSocketsDo,
  )

main = do
  let port = 8080
      hostname = "127.0.0.1"
      maxQueueSize = 2
      backlog = fromIntegral maxQueueSize
  putStrLn $ "Listening on port " ++ show port
  server port hostname backlog

server :: PortNumber -> String -> Int -> IO ()
server port hostname backlog = withSocketsDo $ do
  addr <- resolve hostname port
  sock <- socket (addrFamily addr) Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  bind sock (addrAddress addr)
  listen sock backlog
  forever $ do
    conn <- accept sock
    handle <- socketToHandle (fst conn) ReadWriteMode
    hSetBuffering handle NoBuffering
    request <- hGetLine handle
    putStrLn $ "Received request: " ++ request
    hPutStrLn handle (pack "HTTP/1.1 200 OK\r\nContent-Length: 5\r\n\r\nHello")
    hClose handle

resolve :: String -> PortNumber -> IO AddrInfo
resolve hostname port = do
  let hints = defaultHints {addrSocketType = Stream}
  addr : _ <- getAddrInfo (Just hints) (Just hostname) (Just $ show port)
  return addr
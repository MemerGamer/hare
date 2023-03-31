{-# OPTIONS_GHC -Wno-deprecations #-}

import Control.Monad
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (hPutStrLn, pack, unpack)
import qualified Data.ByteString.Char8 as BSC
import Data.List (intercalate)
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
import System.Directory (doesFileExist)
import System.FilePath (takeExtension, (</>))

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
    let filePath = getFilePath request
    fileExists <- doesFileExist filePath
    if fileExists
      then do
        contents <- BS.readFile filePath
        let contentType = getContentType filePath
        let response =
              intercalate
                "\r\n"
                [ "HTTP/1.1 200 OK",
                  "Content-Type: " ++ contentType,
                  "Content-Length: " ++ show (BS.length contents),
                  "",
                  ""
                ]
        if isImage contentType
          then do
            BSC.hPutStrLn handle (BSC.pack response)
            BS.hPutStr handle contents
          else do
            BSC.hPutStrLn handle (BSC.pack response)
            BSC.hPutStrLn handle (BSC.pack $ unpack contents)
      else do
        hPutStrLn handle (pack "HTTP/1.1 404 Not Found\r\n\r\n")
        hClose handle

isImage :: String -> Bool
isImage contentType =
  contentType == "image/jpeg"
    || contentType == "image/png"
    || contentType == "image/gif"

resolve :: String -> PortNumber -> IO AddrInfo
resolve hostname port = do
  let hints = defaultHints {addrSocketType = Stream}
  addr : _ <- getAddrInfo (Just hints) (Just hostname) (Just $ show port)
  return addr

getFilePath :: String -> FilePath
getFilePath request =
  let parts = words request
      path = parts !! 1
      normalizedPath =
        if head path == '/'
          then tail path
          else path
   in "sites/" </> normalizedPath

getContentType :: FilePath -> String
getContentType path =
  let ext = takeExtension path
   in case ext of
        ".html" -> "text/html"
        ".css" -> "text/css"
        ".js" -> "text/javascript"
        ".png" -> "image/png"
        ".jpg" -> "image/jpeg"
        ".jpeg" -> "image/jpeg"
        _ -> "application/octet-stream"
{-# OPTIONS_GHC -Wno-deprecations #-}

module Server where

import Control.Monad (forever, unless)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import Data.List (intercalate)
import GHC.IO.Handle
  ( BufferMode (LineBuffering, NoBuffering),
    Handle,
    hClose,
    hGetLine,
    hSetBinaryMode,
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
import System.IO (IOMode (ReadMode), hFileSize, withFile)

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
    hSetBinaryMode handle True -- Set handle to binary mode
    request <- hGetLine handle
    putStrLn $ "Received request: " ++ request
    let filePath = getFilePath request
    fileExists <- doesFileExist filePath
    if fileExists
      then do
        let contentType = getContentType filePath
        contents <- BSL.readFile filePath
        let fileSize = BSL.length contents
        let headers = responseHeaders contentType (last $ words filePath) (fromIntegral fileSize)
        putStrLn $ "Sending headers: " ++ headers -- Debug print statement
        BSC.hPut handle (BSC.pack headers)
        loop handle contents
      else do
        putStrLn "File not found" -- Debug print statement
        let redirectHeaders = "HTTP/1.1 302 Found\r\nLocation: /404.html\r\n\r\n"
        BSC.hPut handle (BSC.pack redirectHeaders)
        hClose handle

loop :: Handle -> BSL.ByteString -> IO ()
loop handle contents = do
  let chunkSize = 8192
  let (chunk, rest) = BSL.splitAt chunkSize contents
  unless (BSL.null chunk) $ do
    BSL.hPut handle chunk
    loop handle rest

responseHeaders :: String -> String -> Int -> String
responseHeaders contentType fileName fileSize =
  intercalate
    "\r\n"
    [ "HTTP/1.1 200 OK",
      "Content-Type: " ++ contentType,
      "Content-Disposition: inline; filename=\"" ++ fileName ++ "\"",
      "Content-Length: " ++ show fileSize,
      "",
      ""
    ]

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
      --  in trace ("Normalized path: " ++ normalizedPath) $ "sites/" </> normalizedPath
      filePath =
        if normalizedPath == ""
          then -- redirecting to the index.html if there is no path
            "sites/index.html"
          else "sites" </> normalizedPath
   in filePath

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
        ".gif" -> "image/gif"
        ".bmp" -> "image/bmp"
        ".svg" -> "image/svg+xml"
        ".ico" -> "image/x-icon"
        ".webp" -> "image/webp"
        ".avif" -> "image/avif"
        _ -> "application/octet-stream"

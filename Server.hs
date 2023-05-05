module Server where

-- jelzem hogy ez egy haskell modul

import Control.Monad (forever, unless)
-- Controll.Monad
-- forever ez a függvény vár IO actiont és a vételenségig ismétli
-- unless - vár egy IO actiont és egy booleant és ha a boolean hamis akkor hajtja végre az IO actiont
import qualified Data.ByteString.Char8 as BSC
-- ez a modul ByteStringekkel dolgozik (byte sorozatokkal lényegében) amelyek részei az ASCII karaktereknek
import qualified Data.ByteString.Lazy as BSL
-- ez a modul hasonló az előzőhöz, viszont ez csak akkor dolgozik a bytestringekkel amikor rájuk szükségük van

import Data.List (intercalate)
-- listák manipulálásához szükséges függvények
-- intercalate: összefűzi a listaelemeket adott szeparátorral
import GHC.IO.Handle
  ( BufferMode (LineBuffering, NoBuffering),
    Handle,
    hClose,
    hGetLine,
    hSetBinaryMode,
    hSetBuffering,
  )
-- Handlekkel kapcsolatos függvények csomaga ; (kapcsolódás fileok és/vagy folyamatok között)
-- BufferMode: bufferelési mód handlek esetében
-- Handle: reprezentál egy Handlet
-- hClose: bezár egy handlet
-- hGetLine: kiolvas egy sort a handleből
-- hSetBinaryMode: beállítja a bináris módot a handlenek
-- hSetBuffering: beállítja a bufferelési módot a handlenek
import GHC.IO.IOMode (IOMode (ReadWriteMode))
-- specifikáció file handle mód megnyitására
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
-- A Network.Socket modulban találhatók azok a funkciók, amelyekkel hálózati szolgáltatásokat lehet elérni.
-- Például:
-- socket: létrehoz egy socketet
-- bind: hozzárendeli az adott socketet egy címhez
-- listen: beállítja a socketet várakozó állapotba
-- AddrInfo típus:  az IP címek és portok információit tartalmazza
-- PortNumber típus: az IP portok számát jelöli
-- SocketType típus: az adatátviteli protokollokat határozza meg
-- SocketOption típus: a socket opcióit adja meg
-- meghívtam a PortNumber típust a Network.Socket moduleból
-- bővebben a típusról és a moduleról:
-- https://hackage.haskell.org/package/network-3.1.2.8/docs/Network-Socket.html
-- https://hackage.haskell.org/package/network-3.1.2.8/docs/Network-Socket.html#g:17

import System.Directory (doesFileExist)
import System.FilePath (takeExtension, (</>))
import System.IO (IOMode (ReadMode), hFileSize, withFile)
import Prelude

-- A System.Directory: függvények melyek a fájlok kezelésére szolgálnak
-- doesFileExist: megvizsgálja, hogy létezik-e adott fájl az adott útvonalon
-- A System.FilePath modul: dolgozhatunk a fájlnevekkel és az útvonalakkal
-- </> operátorral: összefűzhetjük az útvonalakat
-- takeExtension függvénnyel: kinyerhetjük a fájlkiterjesztést

server :: PortNumber -> String -> Int -> IO a
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

-- A szerver várakozik bejövő HTTP kérésekre egy megadott porton és hoszton,
-- majd ezeket a kéréseket kezeli azzal, hogy vagy visszaadja a kérésre válaszul egy fájl tartalmát,
-- vagy átirányít a "404 Not Found" oldalra, amennyiben nem talál ilyen filet.
-- Bemenetül kap:
--  PortNumber értéket amin várakozni fog a kérésekre
--  String ami a hoszt név
--  Int ami a maximális várakozási sort jelöli
--  a withSocketsDo leellenőrzni, hogy a socketet megfelelően vannak-e inicializálva
--  ezután a hoszt nevet és a portot egy AddrInfo struktútába rakjuk amelyet felhasznál a szerver
--  hogy létrehozzon egy Streamet a socket függvény segítségével
--  a setSocketOption lehetővé teszi a port újra felhasználását amennyiben a szerver
--  hírtelen leálláskor nem tudná felszabadítani azt
--  ezután a socketet hozzá bindeoljuk a megflelő címhez ami az AddrInfo struktúrában található
--  és a liste fgv. el kezdi figyelni a bejövő http kéréseket a szerverre
-- a forever loopban ha minden rendben van a bejövő kéréseket
-- megfelelő módon szét tagoljuk illetve feldolgozzuk segéd függvényekkel
-- és az általuk adott választ visszaküldjük http protokollon keresztül a kliensnek

loop :: Handle -> BSL.ByteString -> IO ()
loop handle contents = do
  let chunkSize = 16384
  let (chunk, rest) = BSL.splitAt chunkSize contents
  unless (BSL.null chunk) $ do
    BSL.hPut handle chunk
    loop handle rest

-- Ez a függvény egy egyszerű ciklust implementál,
-- amely adatblokkokat küld a hálózatra adott méretű "darabokban".
-- A függvény két paramétert kap:
-- egy Handle típusú értéket, amely a hálózati kommunikációhoz használt kezelő,
-- és egy BSL.ByteString típusú értéket, amely a küldendő adatokat tartalmazza.
--
-- A ciklusban egy adott "darabméret" (chunkSize) értéke kerül inicializálásra,
-- majd a küldendő adatokat felosztja két részre a splitAt függvénnyel.
-- Az első részt (chunk) elküldi a hálózatra a hPut függvénnyel, majd
-- rekurzívan meghívja a loop függvényt a második részre (rest), amíg az összes adatot elküldi.

-- Az unless függvénnyel biztosítja, hogy a ciklus csak akkor fusson,
-- ha az adott "darab" nem üres.
-- A BSL.null függvénnyel ellenőrzi, hogy az adott darab üres-e,
-- és ha nem, akkor elküldi a hálózatra, majd rekurzívan meghívja a loop függvényt
-- a fennmaradó adatokra.
-- A ciklus akkor ér véget, amikor az összes adatot elküldte a hálózatra.

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

-- Ez a függvény a válaszüzenet HTTP fejléceit állítja elő,
-- amelyek tartalmazzák a kérésre adandó választ.

-- A függvény három paramétert vár:
-- contentType (a visszaküldendő fájl típusa),
-- fileName (a visszaküldendő fájl neve),
-- és fileSize (a visszaküldendő fájl mérete).

-- Az intercalate függvénnyel a fejléceknek
-- egy szövegsorozatot ad vissza,
-- amely a következő HTTP fejléceket tartalmazza:

-- Az "HTTP/1.1 200 OK" státuszüzenet, amely jelzi, hogy a kérés sikeres volt.
-- A "Content-Type" fejléc, amely az adott fájl típusát határozza meg.
-- A "Content-Disposition" fejléc,
-- amely az adott fájl elrendezését határozza meg.
-- A "Content-Length" fejléc, amely a visszaküldött adatok méretét határozza meg.
-- Két üres sor, amelyekkel a fejlécek véget érnek.
-- A fejlécek szövegsorozata visszatérési értékként kerül visszaadásra.

resolve :: String -> PortNumber -> IO AddrInfo
resolve hostname port = do
  let hints = defaultHints {addrSocketType = Stream}
  addr : _ <- getAddrInfo (Just hints) (Just hostname) (Just $ show port)
  return addr

-- Ez a függvény egy adott hostname és port érték alapján
-- visszaad egy AddrInfo típusú értéket az IO monádban.

-- A függvényben először létrehozunk egy hints értéket,
-- amely a defaultHints függvényből származik,
-- és beállítjuk a Stream értéket az addrSocketType mezőben,
-- ami azt jelzi, hogy TCP/IP stream socket-et használunk.

-- Ezt követően meghívjuk a getAddrInfo függvényt,
-- amely az adott hostname és port alapján kérdez le egy vagy több AddrInfo értéket
-- a rendszer szolgáltatásaitól.
-- Az első AddrInfo értéket kiválasztjuk a listából,
-- majd visszaadjuk ezt az értéket az IO monádban.

getFilePath :: String -> FilePath
getFilePath request =
  let parts = words request
      path = parts !! 1
      normalizedPath =
        if head path == '/'
          then tail path
          else path
      filePath =
        if normalizedPath == ""
          then -- redirecting to the index.html if there is no path
            "sites/index.html"
          else "sites" </> normalizedPath
   in filePath

-- Ez a függvény a kérésre ad válaszul egy elérési útvonalat a megfelelő fájlnak a szerveren.
-- Bemeneti paraméterként egy Stringet kap ami a kérést tartalmazza.
-- A függvény szét bontja a stringet szavakra és megpróbálja megtalálni a sites könvtáron belül
-- a keresett fileokat. Alapértelmezetten az index.html-t adja vissza
-- amennyiben nem volt megadva kérés az elérési útvonalra vonatkozóan.

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

-- A fenti függvény file kiterjesztés alapján meghatározza
-- egy fájl HTTP kérés beli típusát és ezt adja vissza válaszként.

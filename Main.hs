#!/usr/bin/env runhaskell

import Server (server)

-- meghívtam a Server moduleból a server függvényt

-- main függvény, a kód futtatásakor automatikusan meghívódik
main :: IO ()
main = do
  -- létrhozzuk a portot, jelen esetben 5050
  let port = 5050
      hostname = "127.0.0.1" -- a hostname a jelenlegi alapértelmezett ip cím, lehetne localhost is
      maxQueueSize = 2 -- definiáljuk a max sor hosszat
      backlog = fromIntegral maxQueueSize -- létrehozzuk a backlogot a max sor hosszból
      -- klikkelhető url létrehozása:
      -- létrehozzuk az url változót ami tartalmazz a portot és az elérési útvonalat
      -- az url tartalma: http://localhost:5050
      url = "http://localhost:" ++ show port
      -- alapértelmezetten nem klikkelhető, de ansi kódok segítségével klikkelhető linké válik
      -- forrás: https://gist.github.com/egmontkob/eb114294efbcd5adb1944c9f3cb5feda
      -- megjegyzés: \e]8;; és \x1b]8;; ugyanazt jelenti, én az utóbbit preferálom, itt is azt használtam
      hyperlink = "\x1b]8;;" ++ url ++ "\a" ++ url ++ "\x1b]8;;\a"
  -- miért ismétlődik az url változó kétszer?
  -- hasonlóan a html horgonyhoz ( <a href="#"></a> ) ansi kódok esetében is kétszer kell tartalmazni az urlt
  -- ha hiányzik az első url akkor nem mutat sehová a link, ha hiányzik a második url akkor nem jelez ki semmit
  -- pl.
  -- html: <a href="http://example.com">google.com</a>
  -- ansi kód: "\x1b]8;;http://example.com\ahttp://example.com\x1b]8;;\a"
  putStrLn $ "Listening on: " ++ hyperlink
  -- kiirja terminálra az üzenetet hogy vár kéréseket az adott url-n
  server port hostname backlog

-- meghívja a server függvényt a port, hostname, és backlog változókkal

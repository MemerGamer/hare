import Network.Socket (PortNumber)
import Server (server)

main :: IO ()
main = do
  let port = 5050
      hostname = "127.0.0.1"
      maxQueueSize = 2
      backlog = fromIntegral maxQueueSize
      -- created clickable url with ansi codes
      url = "http://localhost:" ++ show port
      hyperlink = "\x1b]8;;" ++ url ++ "\a" ++ url ++ "\x1b]8;;\a"
  putStrLn $ "Listening on: " ++ hyperlink
  server port hostname backlog

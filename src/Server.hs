module Server (runServer) where
import Handle
import Database
import Network.Gopher (runGopher, defaultConfig, cServerPort, cLogHandler, GopherLogHandler, fromGopherLogStr)

logHandler :: GopherLogHandler
logHandler level str = do
  putStr $ show level ++ ": "
  putStrLn $ fromGopherLogStr str

runServer :: IO ()
runServer = do
  _ <- initializeDatabase
  putStrLn "Starting spacecookie server"
  runGopher (defaultConfig { cServerPort = 7000, cLogHandler = Just logHandler }) handler
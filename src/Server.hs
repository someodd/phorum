{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Server (runServer) where

import Handle
import Database
import Network.Gopher (runGopher, defaultConfig, cServerPort, cLogHandler, GopherLogHandler, fromGopherLogStr, GopherConfig (cServerName))
import Config
import Data.Text.Encoding (encodeUtf8)
import Network.Gopher.Util (dropPrivileges)
import qualified Data.Text as T
import System.Posix.User (getEffectiveUserID)

logHandler :: GopherLogHandler
logHandler level str = do
  putStr $ show level ++ ": "
  putStrLn $ fromGopherLogStr str

-- | Main command to run the Gopher/phorum server.
runServer :: Config -> IO ()
runServer config = do
  _ <- initializeDatabase config
  putStrLn "Starting spacecookie server"
  
  -- Check if running as root before dropping privileges
  euid <- getEffectiveUserID
  if euid == 0
    then do
      _ <- dropPrivileges (T.unpack config.daemon.runAsUser)
      return ()
    else putStrLn "Warning: Not running as root, skipping privilege drop."

  runGopher (defaultConfig { cServerName = encodeUtf8 config.daemon.name, cServerPort = fromIntegral config.daemon.port, cLogHandler = Just logHandler }) (handler config)

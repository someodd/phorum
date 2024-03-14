{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Server (runServer) where

import Handle
import Database
import Network.Gopher (runGopher, defaultConfig, cServerPort, cLogHandler, GopherLogHandler, fromGopherLogStr, GopherConfig (cServerName))
import Config
import Data.Text.Encoding (encodeUtf8)

logHandler :: GopherLogHandler
logHandler level str = do
  putStr $ show level ++ ": "
  putStrLn $ fromGopherLogStr str

runServer :: Config -> IO ()
runServer config = do
  _ <- initializeDatabase config
  putStrLn "Starting spacecookie server"
  runGopher (defaultConfig { cServerName = encodeUtf8 config.spacecookie.name, cServerPort = fromIntegral config.spacecookie.port, cLogHandler = Just logHandler }) (handler config)
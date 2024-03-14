{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Server (runServer) where

import Handle
import Database
import Network.Gopher (runGopher, defaultConfig, cServerPort, cLogHandler, GopherLogHandler, fromGopherLogStr)
import Config

logHandler :: GopherLogHandler
logHandler level str = do
  putStr $ show level ++ ": "
  putStrLn $ fromGopherLogStr str

runServer :: Config -> IO ()
runServer config = do
  _ <- initializeDatabase config
  putStrLn "Starting spacecookie server"
  runGopher (defaultConfig { cServerPort = 7000, cLogHandler = Just logHandler }) (handler config)
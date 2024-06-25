{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module CLI (entryPoint) where

import Server (runServer)
import Database (banIP, unbanIP, listBannedIPs)

import Options.Applicative
import Config

data Command
  = LaunchServer
  | BanIP { postId :: Integer, reason :: String, deletePost :: Bool }
  | UnbanIP { ipAddr :: String }
  | ListBans

parseListBans :: Parser Command
parseListBans = pure ListBans

parseLaunchServer :: Parser Command
parseLaunchServer = pure LaunchServer

parseUnbanIP :: Parser Command
parseUnbanIP = UnbanIP
  <$> strOption
      ( long "ip"
     <> metavar "IPADDRESS"
     <> help "IP address to unban" )

parseBanIP :: Parser Command
parseBanIP = BanIP
  <$> option auto
      ( long "post"
     <> metavar "POSTID"
     <> help "ID of the post associated with the IP to ban" )
  <*> strOption
      ( long "reason"
     <> metavar "REASON"
     <> help "Reason for banning the IP" )
  <*> switch
      ( long "delete"
     <> help "Delete the post as well as banning the IP" )

data ConfigCommand = ConfigCommand
  { configPath :: String
  , command :: Command
  }

parseConfigCommand :: Parser ConfigCommand
parseConfigCommand = ConfigCommand
  <$> strOption
      ( long "config"
     <> metavar "CONFIG"
     <> help "Path to the configuration file" )
  <*> commands

entryPoint :: IO ()
entryPoint = do
  configCommand <- execParser opts
  config <- loadConfig configCommand.configPath
  runCLI config (configCommand.command)
 where
  opts = info (parseConfigCommand <**> helper)
    ( fullDesc
    <> progDesc "Run server or ban an IP"
    <> header "server-cli - a simple CLI for managing your server" )

commands :: Parser Command
commands = subparser
  ( command "launch" (info parseLaunchServer (progDesc "Launch the server"))
 <> command "ban" (info parseBanIP (progDesc "Ban an IP by post ID"))
 <> command "unban" (info parseUnbanIP (progDesc "Unban an IP address"))
 <> command "list_bans" (info parseListBans (progDesc "List all banned IPs"))
  )

runCLI :: Config -> Command -> IO ()
runCLI config LaunchServer = runServer config
runCLI config (BanIP postId reason delete) = do
  putStrLn $ "Banning IP for post: " ++ show postId ++ " for reason: " ++ reason
  banIP config postId reason delete
  putStrLn "Done."
runCLI config (UnbanIP ipAddr) = do
  putStrLn $ "Unbanning IP: " ++ ipAddr
  unbanIP config ipAddr
  putStrLn "IP unbanned successfully."
runCLI config ListBans = do
  putStrLn "Listing all banned IPs:"
  bannedIPs <- listBannedIPs config.databaseConnection
  mapM_ (\(ip, reason) -> putStrLn $ ip ++ " - " ++ reason) bannedIPs

-- FIXME: redundant
-- Assuming you have a function to load the config from the path
loadConfig :: String -> IO Config
loadConfig path = getConfig path -- Modify this to actually load the config from the given path

{- | Command line interface.

-}
{-# LANGUAGE OverloadedStrings #-}

module CLI (entryPoint) where

import Server (runServer)
import Database (banIP, unbanIP, listBannedIPs)

import Options.Applicative

-- Some say partials are bad, but I'm lazy and I frankly just generate a lot of the CLI stuff.
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

entryPoint :: IO ()
entryPoint = runCLI =<< execParser opts
  where
    opts = info (commands <**> helper)
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

runCLI :: Command -> IO ()
runCLI LaunchServer = runServer
runCLI (BanIP postId reason delete) = do
  putStrLn $ "Banning IP for post: " ++ show postId ++ " for reason: " ++ reason
  banIP postId reason delete
  putStrLn "Done."
runCLI (UnbanIP ipAddr) = do
  putStrLn $ "Unbanning IP: " ++ ipAddr
  unbanIP ipAddr
  putStrLn "IP unbanned successfully."
runCLI ListBans = do
  putStrLn "Listing all banned IPs:"
  bannedIPs <- listBannedIPs
  mapM_ (\(ip, reason) -> putStrLn $ ip ++ " - " ++ reason) bannedIPs


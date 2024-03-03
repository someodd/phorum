{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Database where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Control.Exception (bracket)
import Control.Monad (when)
import Data.Maybe (listToMaybe)
import Database.PostgreSQL.Simple.FromRow
import Data.Time (LocalTime)


import Config
import Data.Functor (void)

-- FIXME: just define PostDB
data PostInsert = PostInsert {
    message   :: String,
    replyTo   :: Maybe Integer,
    postIP    :: String       -- New field for IP address
} deriving (Show)

data PostDB = PostDB {
    postId      :: Integer,
    message     :: String,
    replyTo     :: Maybe Integer,
    postIP      :: String,
    createdAt   :: LocalTime,
    bannedForPost :: Bool
} deriving (Show)

-- FIXME: could all be more generic, right?
-- Making Post an instance of ToRow to facilitate insertion
-- Adjust the ToRow instance to include the new fields
instance ToRow PostInsert where
    toRow p = [toField (p.message), toField (p.replyTo), toField (p.postIP)]

instance FromRow PostDB where
    fromRow = PostDB <$> field <*> field <*> field <*> field <*> field <*> field

-- Function to connect to your specific database
connectDb :: IO Connection
connectDb = connect defaultConnectInfo {
    connectHost = "localhost",
    connectPort = 5432,
    connectUser = "your_username",
    connectPassword = "your_password",
    connectDatabase = "your_database"
}

-- Function to get all threads, sorted by the freshest activity
getThreadsSortedByFreshest :: IO [PostDB]
getThreadsSortedByFreshest = bracket connectDb close $ \conn -> do
    query_ conn $
        "WITH ThreadActivity AS ( \
        \    SELECT \
        \        COALESCE(replyTo, id) AS thread_id, \
        \        MAX(id) OVER (PARTITION BY COALESCE(replyTo, id)) AS max_id \
        \    FROM app_schema.posts \
        \), RankedThreads AS ( \
        \    SELECT \
        \        thread_id, \
        \        RANK() OVER (ORDER BY max_id DESC) \
        \    FROM ThreadActivity \
        \    GROUP BY thread_id, max_id \
        \) \
        \SELECT p.id, p.message, p.replyTo, p.postIP, p.createdAt, p.bannedForPost \
        \FROM app_schema.posts p \
        \JOIN RankedThreads rt ON p.id = rt.thread_id \
        \WHERE p.replyTo IS NULL \
        \ORDER BY rt.rank" :: IO [PostDB]

-- Function to check and maintain the maximum number of threads
maintainMaximumThreads :: IO ()
maintainMaximumThreads = bracket connectDb close $ \conn -> do
    -- Count the number of threads (posts with replyTo IS NULL)
    [Only threadCount] <- query_ conn "SELECT COUNT(*) FROM app_schema.posts WHERE replyTo IS NULL" :: IO [Only Integer]
    when (threadCount > maximumThreads) $ do
        -- Find the thread with the smallest ID (oldest thread)
        [Only smallestThreadId] <- query_ conn "SELECT id FROM app_schema.posts WHERE replyTo IS NULL ORDER BY id ASC LIMIT 1" :: IO [Only Integer]
        -- Delete all replies to the thread about to be deleted. not needed because cascade. FIXME
        --execute conn "DELETE FROM posts WHERE replyTo = ?" (Only smallestThreadId)
        -- Delete the thread with the smallest ID
        execute conn "DELETE FROM app_schema.posts WHERE id = ?" (Only smallestThreadId)
        putStrLn $ languageDeletedThreadWithId ++ show smallestThreadId

-- Counts the number of replies to a given thread ID
countReplies :: Integer -> IO Int
countReplies threadId = bracket connectDb close $ \conn -> do
    [Only count] <- query conn "SELECT COUNT(*) FROM app_schema.posts WHERE replyTo = ?" (Only threadId) :: IO [Only Int]
    return count

{- | Get three latest replies to a given thread, but return them in ascending order (freshest last).

-}
getThreeLatestReplies :: Integer -> IO [PostDB]
getThreeLatestReplies threadId = bracket connectDb close $ \conn -> do
    query conn "SELECT id, message, replyTo, postIP, createdAt, bannedForPost FROM ( \
                \SELECT id, message, replyTo, postIP, createdAt, bannedForPost \
                \FROM app_schema.posts \
                \WHERE replyTo = ? \
                \ORDER BY id DESC \
                \LIMIT 3) AS latest_replies \
                \ORDER BY id ASC" (Only threadId) :: IO [PostDB]


-- Function to insert a new Post into the database
insertPost :: PostInsert -> IO (Either String Integer)
insertPost post = bracket connectDb close $ \conn -> do
    -- Check if the postIP is in the banned_ips table
    bannedIps <- query conn "SELECT reason FROM app_schema.banned_ips WHERE ip = ?" (Only (post.postIP)) :: IO [Only String]
    if null bannedIps
    then do
        -- IP is not banned, proceed with insertion
        result <- query conn "INSERT INTO app_schema.posts (message, replyTo, postIP) VALUES (?, ?, ?) RETURNING id" post :: IO [Only Integer]
        case result of
            [Only i] -> return $ Right i
            _ -> return $ Left languageFailedToInsertPost
    else return $ Left $ languageYouWereBannedLabel ++ (fromOnly $ head bannedIps)

-- Function to get a thread by ID (replyTo must be NULL)
getThreadById :: Integer -> IO (Maybe PostDB)
getThreadById postId = bracket connectDb close $ \conn -> do
    -- Query to select the post where id matches and replyTo is NULL
    posts <- query conn "SELECT id, message, replyTo, postIP, createdAt, bannedForPost FROM app_schema.posts WHERE id = ? AND replyTo IS NULL" (Only postId) :: IO [PostDB]
    -- Convert the list to Maybe Post (Nothing if the list is empty, Just post if not)
    return $ listToMaybe posts

-- Function to get all replies to a specific thread ID
getRepliesByThreadId :: Integer -> IO [PostDB]
getRepliesByThreadId threadId = bracket connectDb close $ \conn -> do
    -- Query to select all posts where replyTo matches the given thread ID
    query conn "SELECT id, message, replyTo, postIP, createdAt, bannedForPost FROM app_schema.posts WHERE replyTo = ?" (Only threadId) :: IO [PostDB]

-- Function to get all original threads
getAllThreads :: IO [PostDB]
getAllThreads = bracket connectDb close $ \conn -> do
    -- Query to select all posts where replyTo is NULL
    query_ conn "SELECT id, message, replyTo, postIP, createdAt, bannedForPost FROM app_schema.posts WHERE replyTo IS NULL" :: IO [PostDB]

banIP :: Integer -> String -> Bool -> IO ()
banIP postId reason delete = bracket connectDb close $ \conn -> do
    -- Fetch the post's IP using postId
    post <- query conn "SELECT postIP FROM app_schema.posts WHERE id = ?" (Only postId) :: IO [Only String]
    case post of
      [Only ip] -> do
        -- Insert into banned_ips (assume reason is a placeholder here)
        execute conn "INSERT INTO app_schema.banned_ips (ip, reason) VALUES (?, ?) ON CONFLICT (ip) DO NOTHING" (ip, reason)
        if delete
        then -- Delete the post
             void $ execute conn "DELETE FROM app_schema.posts WHERE id = ?" (Only postId)
        else -- Mark the post as bannedForPost
             void $ execute conn "UPDATE app_schema.posts SET bannedForPost = TRUE WHERE id = ?" (Only postId)
        putStrLn $ languageIpBannedSuccessfully ++ ip
      _ -> putStrLn languagePostNotFoundOrIpMissing

-- Function to unban an IP
unbanIP :: String -> IO ()
unbanIP ip = bracket connectDb close $ \conn -> do
    -- Remove the IP from the banned_ips table
    void $ execute conn "DELETE FROM app_schema.banned_ips WHERE ip = ?" (Only ip)
    
    -- Optionally, reset the bannedForPost flag for posts associated with this IP
    -- This step depends on your application's logic and whether you want to retain
    -- the information that a post was once associated with a banned IP.
    void $ execute conn "UPDATE app_schema.posts SET bannedForPost = FALSE WHERE postIP = ?" (Only ip)
    
    putStrLn $ languageIpUnbannedSuccess ++ ip

-- Assuming the import of necessary modules and definitions above
listBannedIPs :: IO [(String, String)]
listBannedIPs = bracket connectDb close $ \conn -> do
    query_ conn "SELECT ip, reason FROM app_schema.banned_ips" :: IO [(String, String)]


initializeDatabase :: IO ()
initializeDatabase = bracket connectDb close $ \conn -> do
    execute_ conn "CREATE SCHEMA IF NOT EXISTS app_schema;"
    execute_ conn "SET search_path TO app_schema;"

    let createPostsTableSQL = "CREATE TABLE IF NOT EXISTS app_schema.posts (\
                              \id SERIAL PRIMARY KEY, \
                              \message VARCHAR(240) NOT NULL, \
                              \replyTo INTEGER, \
                              \postIP VARCHAR(60), \
                              \createdAt TIMESTAMP WITHOUT TIME ZONE DEFAULT NOW() NOT NULL, \
                              \bannedForPost BOOLEAN DEFAULT FALSE, \
                              \FOREIGN KEY (replyTo) REFERENCES posts(id) ON DELETE CASCADE)"
    execute_ conn createPostsTableSQL

    let createBannedIPsTableSQL = "CREATE TABLE IF NOT EXISTS app_schema.banned_ips (\
                                   \ip VARCHAR(60) PRIMARY KEY, \
                                   \reason TEXT)"
    execute_ conn createBannedIPsTableSQL

    putStrLn "Database initialized: Tables created or already exist in app_schema."

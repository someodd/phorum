{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Database where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Control.Exception (bracket)
import Control.Monad (when, join)
import Data.Maybe (listToMaybe, isNothing, isJust)
import Database.PostgreSQL.Simple.FromRow
import Data.Time.Clock


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
    createdAt   :: UTCTime,
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

{-| 'insertPost' is a function that inserts a new post into the database.

It takes as input a 'PostInsert' record, which contains the message, replyTo, and
postIP.

The function first checks if the postIP is in the banned_ips table. If it is, the function
returns a Left value with the ban reason.

If the postIP is not in the banned_ips table, the function checks if the IP has made a
reply in the last (configurable amount of) n minutes or a thread in the last (configurable
amount of) x minutes. If it has, the function returns a Left value with an error message.

If the IP has not made a reply in the last minute or a thread in the last five minutes,
the function proceeds with the insertion of the post into the database. If the insertion
is successful, the function returns a Right value with the id of the inserted post. If the
insertion is not successful, the function returns a Left value with an error.
message.

-}
insertPost :: PostInsert -> IO (Either String Integer)
insertPost post = bracket connectDb close $ \conn -> do
    -- Check if the postIP is in the banned_ips table
    bannedIps <- query conn "SELECT reason FROM app_schema.banned_ips WHERE ip = ?" (Only (post.postIP)) :: IO [Only String]
    if null bannedIps
    then do
        -- IP is not banned, check the rate limits for replies and threads
        lastReply <- checkRateReply conn post.postIP rateLimitMinutesNewReply
        lastThread <- checkRateThread conn post.postIP rateLimitMinutesNewThread
        -- FIXME: this needs to have different message for new threads and replies for ratelimit! different flow
        if (isJust post.replyTo && lastReply) || (isNothing post.replyTo && lastThread)
        then do
            -- IP has not made a reply in the last minute or a thread in the last five minutes, proceed with insertion
            result <- query conn "INSERT INTO app_schema.posts (message, replyTo, postIP) VALUES (?, ?, ?) RETURNING id" post :: IO [Only Integer]
            case result of
                [Only i] -> return $ Right i
                _ -> return $ Left languageFailedToInsertPost
        else return $ Left languagePostRateLimitExceeded
    else return $ Left $ languageYouWereBannedLabel ++ (maybe "" fromOnly $ listToMaybe bannedIps)

-- | Returns True if the IP has *not* made a reply in the last n minutes.
checkRateReply :: Connection -> String -> Int -> IO Bool
checkRateReply conn ip minutes = do
    result <- query conn "SELECT EXISTS(SELECT 1 FROM app_schema.posts WHERE postIP = ? AND replyTo IS NOT NULL AND createdAt > NOW() - INTERVAL '? minutes')" (ip, minutes) :: IO [Only Bool]
    return $ not $ maybe False id $ listToMaybe $ map fromOnly result

-- | Returns True if the IP has *not* made a thread in the last n minutes.
checkRateThread :: Connection -> String -> Int -> IO Bool
checkRateThread conn ip minutes = do
    result <- query conn "SELECT EXISTS(SELECT 1 FROM app_schema.posts WHERE postIP = ? AND replyTo IS NULL AND createdAt > NOW() - INTERVAL '? minutes')" (ip, minutes) :: IO [Only Bool]
    return $ not $ maybe False id $ listToMaybe $ map fromOnly result

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

    -- FIXME/NOTE: is it necessary to have time zone? excess data? literally just doing
    -- that instead of local time/without because it makes the haskell side easier...
    let createPostsTableSQL = "CREATE TABLE IF NOT EXISTS app_schema.posts (\
                              \id SERIAL PRIMARY KEY, \
                              \message VARCHAR(240) NOT NULL, \
                              \replyTo INTEGER, \
                              \postIP VARCHAR(60), \
                              \createdAt TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL, \
                              \bannedForPost BOOLEAN DEFAULT FALSE, \
                              \FOREIGN KEY (replyTo) REFERENCES posts(id) ON DELETE CASCADE)"
    execute_ conn createPostsTableSQL

    let createBannedIPsTableSQL = "CREATE TABLE IF NOT EXISTS app_schema.banned_ips (\
                                   \ip VARCHAR(60) PRIMARY KEY, \
                                   \reason TEXT)"
    execute_ conn createBannedIPsTableSQL

    putStrLn "Database initialized: Tables created or already exist in app_schema."

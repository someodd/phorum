{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Database where

import Config

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Control.Exception (bracket)
import Control.Monad (when, join)
import Data.Maybe (listToMaybe, isNothing, isJust)
import Database.PostgreSQL.Simple.FromRow
import Data.Time.Clock
import Data.Functor (void)
import Data.Text (unpack, pack, Text)

-- FIXME: just define PostDB
data PostInsert = PostInsert {
    message   :: Text,
    replyTo   :: Maybe Integer,
    postIP    :: Text       -- New field for IP address
} deriving (Show)

data PostDB = PostDB {
    postId      :: Integer,
    message     :: Text,
    replyTo     :: Maybe Integer,
    postIP      :: Text,
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
connectDb :: DatabaseConnectionConfig -> IO Connection
connectDb databaseConnection = connect defaultConnectInfo {
    connectHost = unpack $ databaseConnection.connectHost,
    connectPort = fromIntegral $ databaseConnection.connectPort,
    connectUser = unpack $ databaseConnection.connectUser,
    connectPassword = unpack $ databaseConnection.connectPassword,
    connectDatabase = unpack $ databaseConnection.connectDatabase
}

-- Function to get all threads, sorted by the freshest activity
getThreadsSortedByFreshest :: DatabaseConnectionConfig -> IO [PostDB]
getThreadsSortedByFreshest databaseConnection = bracket (connectDb databaseConnection) close $ \conn -> do
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

-- FIXME: maybe make return True if threads were pruned?
-- Function to check and maintain the maximum number of threads
maintainMaximumThreads :: Config -> IO ()
maintainMaximumThreads config = bracket (connectDb config.databaseConnection) close $ \conn -> do
    -- Count the number of threads (posts with replyTo IS NULL)
    [Only threadCount] <- query_ conn "SELECT COUNT(*) FROM app_schema.posts WHERE replyTo IS NULL" :: IO [Only Integer]
    when (threadCount > config.general.maximumThreads) $ do
        -- Find the thread with the smallest ID (oldest thread)
        [Only smallestThreadId] <- query_ conn "SELECT id FROM app_schema.posts WHERE replyTo IS NULL ORDER BY id ASC LIMIT 1" :: IO [Only Integer]
        -- Delete all replies to the thread about to be deleted. not needed because cascade. FIXME
        --execute conn "DELETE FROM posts WHERE replyTo = ?" (Only smallestThreadId)
        -- Delete the thread with the smallest ID
        execute conn "DELETE FROM app_schema.posts WHERE id = ?" (Only smallestThreadId)
        pure ()

-- Counts the number of replies to a given thread ID
countReplies :: DatabaseConnectionConfig -> Integer -> IO Int
countReplies databaseConnection threadId = bracket (connectDb databaseConnection) close $ \conn -> do
    [Only count] <- query conn "SELECT COUNT(*) FROM app_schema.posts WHERE replyTo = ?" (Only threadId) :: IO [Only Int]
    return count

{- | Get three latest replies to a given thread, but return them in ascending order (freshest last).

-}
getThreeLatestReplies :: DatabaseConnectionConfig -> Integer -> IO [PostDB]
getThreeLatestReplies databaseConnection threadId = bracket (connectDb databaseConnection) close $ \conn -> do
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

When posting a reply: ensure the new reply's contents and IP aren't the same as the
preceding reply. The point of this is not just to prevent spam, but mostly to prevent
accidentally reposting by refreshing the page.

When posting a thread: make sure the contents of a thread aren't the same as any other
thread in the database in general. The point of this is to prevent accidental reposting
and to keep the threads at least sort of unique/not bump off a thread for duplicate
content.

Also ensure the IP has not made a reply in the last minute or a thread in the last five minutes.

The function proceeds with the insertion of the post into the database. If the insertion
is successful, the function returns a Right value with the id of the inserted post. If the
insertion is not successful, the function returns a Left value with an error.
message.

-}
insertPost :: Config -> PostInsert -> IO (Either Text Integer)
insertPost config post = bracket (connectDb config.databaseConnection) close $ \conn -> do
    -- Check if the postIP is in the banned_ips table
    bannedIps <- query conn "SELECT reason FROM app_schema.banned_ips WHERE ip = ?" (Only (post.postIP)) :: IO [Only Text]
    if null bannedIps
    then do
        -- IP is not banned, check the rate limits for replies and threads
        lastReply <- checkRateReply conn post.postIP config.general.rateLimitMinutesNewReply
        lastThread <- checkRateThread conn post.postIP config.general.rateLimitMinutesNewThread
        -- Check if the new reply's contents and IP are the same as the preceding reply
        sameReply <- checkSpamReply conn post
        -- Check if the contents of a thread are the same as any other thread in the database
        sameThread <- checkSameThread conn post
        -- Use guards to match various conditions
        case () of
            _ | isJust post.replyTo && not lastReply -> return $ Left config.language.replyRateLimitExceeded
              | isJust post.replyTo && sameReply -> return $ Left config.language.spamReplyError
              | isNothing post.replyTo && not lastThread -> return $ Left config.language.threadRateLimitExceeded
              | isNothing post.replyTo && sameThread -> return $ Left config.language.sameThreadError
              | otherwise -> do
                  -- IP has not made a reply in the last minute or a thread in the last five minutes, proceed with insertion
                  result <- query conn "INSERT INTO app_schema.posts (message, replyTo, postIP) VALUES (?, ?, ?) RETURNING id" post :: IO [Only Integer]
                  case result of
                      [Only i] -> return $ Right i
                      _ -> return $ Left config.language.failedToInsertPost
    else return $ Left $ config.language.youWereBannedLabel <> (maybe "" fromOnly $ listToMaybe bannedIps)

{- | Does this reply share the same contents and IP as the preceding reply (in the thread
it's replying to)?

-}
checkSpamReply :: Connection -> PostInsert -> IO Bool
checkSpamReply conn post = do
  let sql = 
        "SELECT EXISTS ( \
        \  SELECT 1 \
        \  FROM ( \
        \    SELECT * \
        \    FROM app_schema.posts \
        \    WHERE replyTo = ? \
        \    ORDER BY createdAt DESC \
        \    LIMIT 1 \
        \  ) AS latest_reply \
        \  WHERE postIP = ? AND message = ? \
        \) AS match_found;"
  [Only result :: Only Bool] <- query conn sql (post.replyTo, post.postIP, post.message)
  return result

{- | Checks to see if the provided thread post's (OP) contents are the same as any other thread.

-}
checkSameThread :: Connection -> PostInsert -> IO Bool
checkSameThread conn post = do
    [Only result :: Only Bool] <- query conn "SELECT EXISTS (SELECT 1 FROM app_schema.posts WHERE message = ? AND replyTo IS NULL)" (Only post.message)
    return result

-- | Returns True if the IP has *not* made a reply in the last n minutes.
checkRateReply :: Connection -> Text -> Int -> IO Bool
checkRateReply conn ip minutes = do
    result <- query conn "SELECT EXISTS(SELECT 1 FROM app_schema.posts WHERE postIP = ? AND replyTo IS NOT NULL AND createdAt > NOW() - INTERVAL '? minutes')" (ip, minutes) :: IO [Only Bool]
    return $ not $ maybe False id $ listToMaybe $ map fromOnly result

-- | Returns True if the IP has *not* made a thread in the last n minutes.
checkRateThread :: Connection -> Text -> Int -> IO Bool
checkRateThread conn ip minutes = do
    result <- query conn "SELECT EXISTS(SELECT 1 FROM app_schema.posts WHERE postIP = ? AND replyTo IS NULL AND createdAt > NOW() - INTERVAL '? minutes')" (ip, minutes) :: IO [Only Bool]
    return $ not $ maybe False id $ listToMaybe $ map fromOnly result

-- Function to get a thread by ID (replyTo must be NULL)
getThreadById :: DatabaseConnectionConfig -> Integer -> IO (Maybe PostDB)
getThreadById databaseConnection postId = bracket (connectDb databaseConnection) close $ \conn -> do
    -- Query to select the post where id matches and replyTo is NULL
    posts <- query conn "SELECT id, message, replyTo, postIP, createdAt, bannedForPost FROM app_schema.posts WHERE id = ? AND replyTo IS NULL" (Only postId) :: IO [PostDB]
    -- Convert the list to Maybe Post (Nothing if the list is empty, Just post if not)
    return $ listToMaybe posts

-- Function to get all replies to a specific thread ID
getRepliesByThreadId :: DatabaseConnectionConfig -> Integer -> IO [PostDB]
getRepliesByThreadId databaseConnection threadId = bracket (connectDb databaseConnection) close $ \conn -> do
    -- Query to select all posts where replyTo matches the given thread ID
    query conn "SELECT id, message, replyTo, postIP, createdAt, bannedForPost FROM app_schema.posts WHERE replyTo = ?" (Only threadId) :: IO [PostDB]

-- Function to get all original threads
getAllThreads :: DatabaseConnectionConfig -> IO [PostDB]
getAllThreads databaseConnection = bracket (connectDb databaseConnection) close $ \conn -> do
    -- Query to select all posts where replyTo is NULL
    query_ conn "SELECT id, message, replyTo, postIP, createdAt, bannedForPost FROM app_schema.posts WHERE replyTo IS NULL" :: IO [PostDB]

banIP :: Config -> Integer -> String -> Bool -> IO ()
banIP config postId reason delete = bracket (connectDb config.databaseConnection) close $ \conn -> do
    -- Fetch the post's IP using postId
    post <- query conn "SELECT postIP FROM app_schema.posts WHERE id = ?" (Only postId) :: IO [Only Text]
    case post of
      [Only ip] -> do
        -- Insert into banned_ips (assume reason is a placeholder here)
        execute conn "INSERT INTO app_schema.banned_ips (ip, reason) VALUES (?, ?) ON CONFLICT (ip) DO NOTHING" (ip, reason)
        if delete
        then -- Delete the post
             void $ execute conn "DELETE FROM app_schema.posts WHERE id = ?" (Only postId)
        else -- Mark the post as bannedForPost
             void $ execute conn "UPDATE app_schema.posts SET bannedForPost = TRUE WHERE id = ?" (Only postId)
        putStrLn . unpack $ config.language.ipBannedSuccessfully <> ip
      _ -> putStrLn . unpack $ config.language.postNotFoundOrIpMissing

-- Function to unban an IP
unbanIP :: Config -> String -> IO ()
unbanIP config ip = bracket (connectDb config.databaseConnection) close $ \conn -> do
    -- Remove the IP from the banned_ips table
    void $ execute conn "DELETE FROM app_schema.banned_ips WHERE ip = ?" (Only ip)
    
    -- Optionally, reset the bannedForPost flag for posts associated with this IP
    -- This step depends on your application's logic and whether you want to retain
    -- the information that a post was once associated with a banned IP.
    void $ execute conn "UPDATE app_schema.posts SET bannedForPost = FALSE WHERE postIP = ?" (Only ip)
    
    putStrLn . unpack $ (config.language.ipUnbannedSuccess) <> (pack ip)

-- Assuming the import of necessary modules and definitions above
listBannedIPs :: DatabaseConnectionConfig -> IO [(String, String)]
listBannedIPs databaseConnection = bracket (connectDb databaseConnection) close $ \conn -> do
    query_ conn "SELECT ip, reason FROM app_schema.banned_ips" :: IO [(String, String)]


initializeDatabase :: Config -> IO ()
initializeDatabase config = bracket (connectDb config.databaseConnection) close $ \conn -> do
    execute_ conn "CREATE SCHEMA IF NOT EXISTS app_schema;"
    execute_ conn "SET search_path TO app_schema;"

    -- FIXME/NOTE: is it necessary to have time zone? excess data? literally just doing
    -- that instead of local time/without because it makes the haskell side easier...
    let createPostsTableSQL = "CREATE TABLE IF NOT EXISTS app_schema.posts (\
                              \id SERIAL PRIMARY KEY, \
                              \message VARCHAR(?) NOT NULL, \
                              \replyTo INTEGER, \
                              \postIP VARCHAR(60), \
                              \createdAt TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL, \
                              \bannedForPost BOOLEAN DEFAULT FALSE, \
                              \FOREIGN KEY (replyTo) REFERENCES posts(id) ON DELETE CASCADE)"
    execute conn createPostsTableSQL (Only config.general.maximumPostLength)

    let createBannedIPsTableSQL = "CREATE TABLE IF NOT EXISTS app_schema.banned_ips (\
                                   \ip VARCHAR(60) PRIMARY KEY, \
                                   \reason TEXT)"
    execute_ conn createBannedIPsTableSQL

    putStrLn "Database initialized: Tables created or already exist in app_schema."

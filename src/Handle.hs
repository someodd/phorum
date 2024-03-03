-- FIXME: better naming convention.
{- | Handle Gopher Protocol requests.

-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Handle where

import Database
import MenuBuild
import Config
import MenuViews
import TextViews

import Control.Exception (SomeException, catch)
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as B
import Data.Char (isAlphaNum, isSpace)
import Data.List (intercalate, isPrefixOf)
import Data.Maybe (fromMaybe, listToMaybe)
import GHC.Generics (Generic)
import Network.Gopher hiding (GopherFileType (..), GopherMenuItem (..))
import Network.Gopher qualified as Gopher (GopherFileType (..), GopherMenuItem (..))
import Text.Regex.Posix
import Data.Maybe (listToMaybe)
import Data.List (isSuffixOf)
import Data.Word (Word16)
import Text.Printf (printf)
import Data.List (intercalate)


-- Define a data type for the different actions
data Action = ThreadIndex
            | NewThread
            | ViewThreadMenu Integer
            | ViewThreadFile Integer
            | ReplyToThread Integer
            | ViewReplyFile Integer Integer
            | ViewReplyMenu Integer Integer
            | ReplyToReply Integer Integer
            deriving (Show)

-- Function to extract numbers from a string
extractNumbers :: String -> [Integer]
extractNumbers input = map read $ getAllTextMatches $ input =~ ("[0-9]+" :: String) :: [Integer]

{- | Returns the "parts" of a selector we are concerned with.

There are only a few allowed selectors with associated actions

  - `/` - the thread index
  - `/newthread` - query for creating a new thread.
  - `/<thread number>/menu` - for simply viewing the thread and all its replies as a menu
  - `/<thread_number>/file` - for viewing the thread and all its replies as a text file
  - `/<thread_number>/reply` - for replying to the thread with a query
  - `/<thread_number>/<reply_number>/file` - for viewing a reply as a text file.
  - `/<thread_number>/<reply_number>/menu` - for viewing a reply as a menu.
  - `/<thread_number>/<reply_number>/reply - reply to a reply (quotes)

Note that threads and replies share the same ID system--a post is a post, kinda.

-}
decipherSelector :: String -> Maybe Action
decipherSelector selector = case selector of
  "/" -> Just ThreadIndex
  "/newthread" -> Just NewThread
  _ -> case extractNumbers selector of
         [threadNum] -> case () of
                          _ | "/menu" `isSuffixOf` selector -> Just $ ViewThreadMenu threadNum
                            | "/file" `isSuffixOf` selector -> Just $ ViewThreadFile threadNum
                            | "/reply" `isSuffixOf` selector -> Just $ ReplyToThread threadNum
                            | otherwise -> Nothing
         [threadNum, replyNum] -> case () of
                                    _ | "/file" `isSuffixOf` selector -> Just $ ViewReplyFile threadNum replyNum
                                      | "/menu" `isSuffixOf` selector -> Just $ ViewReplyMenu threadNum replyNum
                                      | "/reply" `isSuffixOf` selector -> Just $ ReplyToReply threadNum replyNum
                                      | otherwise -> Nothing
         _ -> Nothing

-- | Things prefixed by "view*Of" are a view for something. they create responses.
viewMenuOfThread :: Integer -> IO GopherResponse
viewMenuOfThread threadId = do
    maybeThread <- getThreadById threadId
    case maybeThread of
        Just post -> do
            replies <- getRepliesByThreadId threadId
            return $ MenuResponse $ menuViewThread post replies
        Nothing ->
            return $ ErrorResponse $ BC.pack $ "There is no thread #" ++ show threadId

{- | Handle creating a response in the form of a text file representing a thread view.

-}
handleThreadFile :: Integer -> IO GopherResponse
handleThreadFile threadId = do
  maybeThread <- getThreadById threadId
  case maybeThread of
    Just post -> do
      replies <- getRepliesByThreadId threadId
      return $ FileResponse $ BC.pack $ textViewThread post replies
    Nothing -> do
        print "print is not thread?!"
        return $ ErrorResponse $ BC.pack $ "There is no thread #" ++ show threadId

-- | Things prefixed by "handleQuery" directly are responding to a request of a broken down selector...
handleQueryNewThread :: GopherRequest -> IO GopherResponse
handleQueryNewThread request = do
    case requestSearchString request of
      Just queryPartTheMessage -> do
        -- may throw error if too long! fixme to return error if so... return (ErrorResponse $ BC.pack "Client sent an empty query.")
        newThreadIdOrFailure <- insertPost $ PostInsert (BC.unpack queryPartTheMessage) Nothing (tupleToIPv6String $ requestClientAddr request)
        case newThreadIdOrFailure of
          Right newThreadId ->
            viewMenuOfThread newThreadId
          Left failureMessage ->
            return (ErrorResponse $ BC.pack failureMessage)
      Nothing ->
        return (ErrorResponse $ BC.pack "Client sent an empty query.")  -- FIXME: should be a gophermap!

-- Converts a Word16 value to a 4-character hexadecimal string
word16ToHex :: Word16 -> String
word16ToHex = printf "%04x"

-- Converts a tuple representing an IPv6 address into its string representation
tupleToIPv6String :: (Word16, Word16, Word16, Word16, Word16, Word16, Word16, Word16) -> String
tupleToIPv6String (w1, w2, w3, w4, w5, w6, w7, w8) =
  intercalate ":" $ map word16ToHex [w1, w2, w3, w4, w5, w6, w7, w8]

-- no validation that replying to real thread?
-- | Things prefixed by "handleQuery" directly are responding to a request of a broken down selector...
handleQueryReplyToThread :: Integer -> GopherRequest -> IO GopherResponse
handleQueryReplyToThread threadId request = do
    case requestSearchString request of
      Just queryPartTheMessage -> do
        -- may throw error if too long! fixme to return error if so... return (ErrorResponse $ BC.pack "Client sent an empty query.")
        postIdOrFailure <- insertPost $ PostInsert (BC.unpack queryPartTheMessage) (Just threadId) (tupleToIPv6String $ requestClientAddr request)
        case postIdOrFailure of
          Right _ ->
            viewMenuOfThread threadId
          Left failureMessage ->
            return (ErrorResponse $ BC.pack failureMessage)
      Nothing ->
        return (ErrorResponse $ BC.pack "Client sent an empty query.")  -- FIXME: should be a gophermap!

handleThreadIndex :: IO GopherResponse
handleThreadIndex = do
    threadPosts <- getThreadsSortedByFreshest
    threadIndex <- menuViewIndex threadPosts
    return $ MenuResponse $ threadIndex

handler :: GopherRequest -> IO GopherResponse
handler request = do
  let selector = BC.unpack $ requestSelector request
  case decipherSelector selector of
    Just (ThreadIndex) -> do
      handleThreadIndex
    Just (NewThread) ->
      handleQueryNewThread request
    Just (ViewThreadMenu threadNumber) ->
      -- handleViewThreadMenu postNumber
      viewMenuOfThread threadNumber
    Just (ViewThreadFile threadNumber) ->
      handleThreadFile threadNumber
    Just (ReplyToThread threadNumber) ->
      handleQueryReplyToThread threadNumber request
      --return (ErrorResponse $ BC.pack "Not yet implemented.")
    Just (ViewReplyFile threadNumber replyNumber) ->
      --handleViewReplyFile threadNumber replyNumber
      return (ErrorResponse $ BC.pack "Not yet implemented.")
    Just (ViewReplyMenu threadNumber replyNumber) ->
      --handleViewReplyMenu threadNumber replyNumber
      return (ErrorResponse $ BC.pack "Not yet implemented.")
    Just (ReplyToReply threadNumber replyNumber) ->
      --handleQueryReplyToReply threadNumber replyNumber request
      return (ErrorResponse $ BC.pack "Not yet implemented.")
    Nothing ->
      return (ErrorResponse $ BC.pack $ "Invalid request:" ++ show selector)
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
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Text (Text, pack)
import qualified Data.Text as T
import Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import System.FilePath ((</>))


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

You can define a selector prefix in case phorum is acting as if it's in a subdirectory.
Otherwise you can pass `""` as the prefix.

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
decipherSelector :: String -> String -> Maybe Action
decipherSelector selectorPrefix selector = case drop (length selectorPrefix) selector of
  "" -> Just ThreadIndex
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
viewMenuOfThread :: Config -> Integer -> IO GopherResponse
viewMenuOfThread config threadId = do
    maybeThread <- getThreadById config.databaseConnection threadId
    case maybeThread of
        Just post -> do
            replies <- getRepliesByThreadId config.databaseConnection  threadId
            return $ MenuResponse $ menuViewThread config post replies
        Nothing ->
            return $ ErrorResponse $ BC.pack $ "There is no thread #" ++ show threadId

{- | Handle creating a response in the form of a text file representing a thread view.

-}
handleThreadFile :: Config -> Integer -> IO GopherResponse
handleThreadFile config threadId = do
  maybeThread <- getThreadById config.databaseConnection threadId
  case maybeThread of
    Just post -> do
      replies <- getRepliesByThreadId config.databaseConnection threadId
      return $ FileResponse $ encodeUtf8 $ textViewThread config post replies
    Nothing -> do
        return $ ErrorResponse $ BC.pack $ "There is no thread #" ++ show threadId

-- | Helper to create an error if the provided text is longer than the maximum post length.
validatePostMaxLength :: Config -> Text -> Either GopherResponse Text
validatePostMaxLength config text = do
  if (T.length text) > (config.general.maximumPostLength)
    then Left $ ErrorResponse $ BC.pack $ printf (T.unpack config.language.postTooLong) config.general.maximumPostLength (T.length text)
    else Right text

validatePostNotEmpty :: Config -> GopherRequest -> Either GopherResponse Text
validatePostNotEmpty config request = do
  let text = decodeUtf8 $ fromMaybe "" $ requestSearchString request
  if T.null text
    then Left $ ErrorResponse $ encodeUtf8 config.language.postEmpty
    else Right text

postFromRequest :: GopherRequest -> Maybe Integer -> PostInsert
postFromRequest request threadReplyingToId =
  PostInsert
    (decodeUtf8 $ fromMaybe "" $ requestSearchString request)
    threadReplyingToId
    (tupleToIPv6String $ requestClientAddr request)

{- | Perform validation on a post request.

Perform error-checks on client data before attempting to connect to database.

Prepares a PostInsert object for insertion into the database based off a GopherRequest.

-}
postValidator :: Config -> GopherRequest -> Maybe Integer -> Either GopherResponse PostInsert
postValidator config request replyPostId  = do
  let validatedPostText = do
        text <- validatePostNotEmpty config request
        validatePostMaxLength config text
  case validatedPostText of
    Left errorResponse ->
      Left errorResponse
    Right _ ->
      Right $ postFromRequest request replyPostId

-- FIXME: could do with more flat structure by having a validation consistency, maybe Either transformers or something
-- | Things prefixed by "handleQuery" directly are responding to a request of a broken down selector...
handleQueryNewThread :: Config -> GopherRequest -> IO GopherResponse
handleQueryNewThread config request = do
  let validatedPost = postValidator config request Nothing
  case validatedPost of
    Left errorResponse ->
      return errorResponse
    Right post -> do
      newThreadId <- insertPost config post
      either (return . ErrorResponse . encodeUtf8) (viewMenuOfThread config) newThreadId

-- Converts a Word16 value to a 4-character hexadecimal string
word16ToHex :: Word16 -> String
word16ToHex = printf "%04x"

-- Converts a tuple representing an IPv6 address into its string representation
tupleToIPv6String :: (Word16, Word16, Word16, Word16, Word16, Word16, Word16, Word16) -> Text
tupleToIPv6String (w1, w2, w3, w4, w5, w6, w7, w8) =
  pack $ intercalate ":" $ map word16ToHex [w1, w2, w3, w4, w5, w6, w7, w8]

-- no validation that replying to real thread?
-- | Things prefixed by "handleQuery" directly are responding to a request of a broken down selector...
handleQueryReplyToThread :: Config -> Integer -> GopherRequest -> IO GopherResponse
handleQueryReplyToThread config threadId request = do
  let validatedPost = postValidator config request (Just threadId)
  case validatedPost of
    Left errorResponse ->
      return errorResponse
    Right post -> do
      replyResult <- insertPost config post
      either (return . ErrorResponse . encodeUtf8) (const $ viewMenuOfThread config threadId) replyResult

handleThreadIndex :: Config -> IO GopherResponse
handleThreadIndex config = do
    threadPosts <- getThreadsSortedByFreshest config.databaseConnection
    threadIndex <- menuViewIndex config threadPosts
    return $ MenuResponse $ threadIndex

handler :: Config -> GopherRequest -> IO GopherResponse
handler config request = do
  let selector = BC.unpack $ requestSelector request
  case decipherSelector (T.unpack config.general.selectorPrefix) selector of
    Just (ThreadIndex) -> do
      handleThreadIndex config
    Just (NewThread) ->
      handleQueryNewThread config request
    Just (ViewThreadMenu threadNumber) ->
      -- handleViewThreadMenu postNumber
      viewMenuOfThread config threadNumber
    Just (ViewThreadFile threadNumber) ->
      handleThreadFile config threadNumber
    Just (ReplyToThread threadNumber) ->
      handleQueryReplyToThread config threadNumber request
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
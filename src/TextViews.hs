-- FIXME: rename FileViews?
{- | Text file presentations which are used as responses to Gopher requests.

Represent things like index view and thread view.

-}

{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
module TextViews
    ( textViewThread
    ) where

import Database
import TextBuild
import Config
import ViewHelpers ( helperPostMeta )

import Data.Text (Text, unpack, take, unlines)
import Prelude hiding (take, unlines)

-- | Convert a `Text` to a `Char`. Defaults to a space.
textToChar :: Text -> Char
textToChar text = 
  case unpack (take 1 text) of
    [c] -> c
    _   -> ' '

{- | Fancy text file presentation for a thread, displaying the original post and all of its replies.

Lots of ASCII art involved.

-}
textViewThread :: Config -> PostDB -> [PostDB] -> Text
textViewThread config op replies =
    let
        threadMetaInfo = helperPostMeta config op
        opBox = textBuildBox
            threadMetaInfo
            op.message
            (textToChar config.fileViews.threadOpBorderHorizontal)
            (textToChar config.fileViews.threadOpBorderVertical)
            (textToChar config.fileViews.threadOpBorderCorner)
            config.fileViews.threadOpMinimumWidth
            config.fileViews.threadOpMaximumWidth
            config.fileViews.threadOpPadding
        replyBoxes = map
            (\reply -> textBuildBox (helperPostMeta config reply)
            reply.message
            (textToChar config.fileViews.threadReplyBorderHorizontal)
            (textToChar config.fileViews.threadReplyBorderVertical)
            (textToChar config.fileViews.threadReplyBorderCorner)
            config.fileViews.threadReplyMinimumWidth
            config.fileViews.threadReplyMaximumWidth
            config.fileViews.threadReplyPadding)
            replies
    in
        unlines $ opBox : replyBoxes
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
import ViewHelpers

{- | Fancy text file presentation for a thread, displaying the original post and all of its replies.

Lots of ASCII art involved.

-}
textViewThread :: PostDB -> [PostDB] -> String
textViewThread op replies =
    let
        threadMetaInfo = helperPostMeta op
        opBox = textBuildBox threadMetaInfo op.message fileThreadOpBorderHorizontal fileThreadOpBorderVertical fileThreadOpBorderCorner fileThreadOpMinimumWidth fileThreadOpMaximumWidth fileThreadOpPadding
        replyBoxes = map (\reply -> textBuildBox (helperPostMeta reply) reply.message fileThreadReplyBorderHorizontal fileThreadReplyBorderVertical fileThreadReplyBorderCorner fileThreadReplyMinimumWidth fileThreadReplyMaximumWidth fileThreadReplyPadding) replies
    in
        unlines $ opBox : replyBoxes
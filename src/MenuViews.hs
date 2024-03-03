{- | Gophermap/menu presentations.

Creates a Gopher menu that can be used as a response to a Gopher request.

Relatedly, please see the `Build` module.

The "menu views" that get exported should have a type of `[Gopher.GopherMenuItem]` or similar.

This also contains gopher menu representations of `PostDB` in certain contexts (functions starting with `menuRepresent`).

Only exports menu views, which are indicated by function names beginning with `menuView`.

-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

module MenuViews
    ( menuViewIndex
    , menuViewThread
    ) where

import Network.Gopher qualified as Gopher (GopherMenuItem (..))

import ViewHelpers
import Database
import MenuBuild
import Config
import qualified MenuBuild as Gopher

{- | A helper function for displaying the `userWasBannedForThisPost` message.

-}
wasBannedHelper :: Bool -> [GopherLine]
wasBannedHelper True = menuBuildInfoLines languageUserWasBannedForThisPost
wasBannedHelper False = menuBuildInfoLines ""

{- | A menu representation of a `PostDB` original/top/first post for the thread view.

Not a reply.

-}
menuRepresentOriginalPost :: PostDB -> [Gopher.GopherMenuItem]
menuRepresentOriginalPost post =
    let
        threadHeading = menuBuildHeadingParticular (threadOpThreadNumberLabel ++ show post.postId) threadOpHeadingDecorLeft threadOpHeadingDecorRight False False
        threadMessage = menuBuildInfoLines post.message
        wasBanned = wasBannedHelper post.bannedForPost
    in
        toGopherMenuItems $ threadHeading ++ threadMessage ++ wasBanned

{- | A menu representation of a `PostDB` reply for the thread view.

-}
menuRepresentThreadReply :: PostDB -> [Gopher.GopherMenuItem]
menuRepresentThreadReply post =
    let
        replyHeading = menuBuildHeadingParticular (threadReplyNumberLabel ++ show post.postId) threadReplyHeadingDecorLeft "" False False
        replyMessage = menuBuildInfoLines post.message
    in
        toGopherMenuItems $ replyHeading ++ replyMessage

{- | A menu representation of a `PostDB` reply for the index view.

-}
menuRepresentIndexReply :: PostDB -> [Gopher.GopherMenuItem]
menuRepresentIndexReply post =
    let
        replyMeta = ("No. " ++ show post.postId) ++ ", " ++ show post.createdAt
        replyHeading = menuBuildHeadingParticular replyMeta indexReplyHeadingDecorLeft indexReplyHeadingDecorRight indexReplyLeadingBreak indexReplyTrailingBreak
        replyMessage = menuBuildInfoLines post.message
    in
        toGopherMenuItems $ replyHeading ++ replyMessage

{- | Menu view for a thread, displaying the original post and all of its replies.

-}
menuViewThread :: PostDB -> [PostDB] -> [Gopher.GopherMenuItem]
menuViewThread post replies =
    let
        originalPostLines = menuRepresentOriginalPost post
        repliesLines = concatMap menuRepresentThreadReply replies
        -- fromJust here bad?! this kind of sucks very bad type handling or whatever
        createReply = toGopherMenuItems $ [menuBuildQueryLine languageThreadReply ("/" ++ show post.postId ++ "/reply")]
        returnToIndex = toGopherMenuItems [menuBuildMenuLine languageReturnToIndex "/"]
        viewThreadFileLink = toGopherMenuItems [menuBuildFileLine languageViewAsFile ("/" ++ show post.postId ++ "/file")]
    in
        originalPostLines ++ repliesLines ++ createReply ++ viewThreadFileLink ++ returnToIndex

{- | Menu view for the thread index, displaying each thread and the latest three replies
for each, along with how many replies were omitted.

-}
menuViewIndex :: [PostDB] -> IO [Gopher.GopherMenuItem]
menuViewIndex posts = do
    summaries <- concatMapM menuRepresentIndexThreadSummary posts
    let
        preamble = toGopherMenuItems $ menuBuildInfoLines indexPreamble
        newThread = toGopherMenuItems [menuBuildQueryLine languageCreateThread "/newthread"]

    pure $ preamble ++ summaries ++ newThread

concatMapM f xs = concat <$> traverse f xs

{- | Basic template logic for the "replies omitted" message for the index.

-}
repliesOmitted :: Int -> [GopherLine]
repliesOmitted omitted
    | omitted == 1 =  menuBuildInfoLines $ languageSingleReplyOmitted
    | omitted > 1 =  menuBuildInfoLines $ show omitted ++ languagePluralRepliesOmitted
    | otherwise = []

{- | A representation of a `PostDB` (post from the database, namely a thread) for the index view.

-}
menuRepresentIndexThreadSummary :: PostDB -> IO [Gopher.GopherMenuItem]
menuRepresentIndexThreadSummary post = do
    replyCount <- countReplies post.postId
    latestReplies <- getThreeLatestReplies post.postId
    let
        threadMetaInfo = helperPostMeta post
        threadHeading = menuBuildHeadingParticular threadMetaInfo indexOpHeadingDecorLeft indexOpHeadingDecorRight True False
        viewThreadMenuLink = menuBuildMenuLine languageViewAsMenu ("/" ++ show post.postId ++ "/menu")
        viewThreadFileLink = menuBuildFileLine languageViewAsFile ("/" ++ show post.postId ++ "/file")
        threadMessage = menuBuildInfoLines $ "\n" ++ post.message ++ "\n"
        totalRepliesStatus = repliesOmitted $ replyCount - (length latestReplies)
        latestRepliesLines = concatMap menuRepresentIndexReply latestReplies

    pure $ (toGopherMenuItems $ threadHeading ++ [viewThreadMenuLink, viewThreadFileLink] ++ threadMessage ++ totalRepliesStatus) ++ latestRepliesLines
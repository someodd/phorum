{- | Gophermap/menu presentations.

Creates a Gopher menu that can be used as a response to a Gopher request.

Relatedly, please see the `Build` module.

The "menu views" that get exported should have a type of `[Gopher.GopherMenuItem]` or similar.

This also contains gopher menu representations of `PostDB` in certain contexts (functions starting with `menuRepresent`).

Only exports menu views, which are indicated by function names beginning with `menuView`.

-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module MenuViews
    ( menuViewIndex
    , menuViewThread
    ) where

import ViewHelpers
import Database
import MenuBuild
import Config
import LinkDetection

import Data.Text (pack, Text)
import Data.Maybe (fromMaybe)
import Control.Applicative ((<|>))
import Network.Gopher qualified as Gopher (GopherMenuItem (..))

-- | Like a hook that changes the message for presentation purposes.
messagePresenter :: Config -> Text -> [GopherLine]
messagePresenter config message
    = fromMaybe (menuBuildInfoLines message) $ 
        ((:[]) <$> parseGopherURI message)
        <|> ((:[]) <$> parseHttpURI message)
        <|> (menuBuildInfoLines <$> findSpecialCode config.specialCodes message)

{- | A helper function for displaying the `userWasBannedForThisPost` message.

-}
wasBannedHelper :: LanguageConfig -> Bool -> [GopherLine]
wasBannedHelper language True = menuBuildInfoLines language.userWasBannedForThisPost
wasBannedHelper _ False = menuBuildInfoLines ""

{- | A menu representation of a `PostDB` original/top/first post for the thread view.

Not a reply.

-}
menuRepresentOriginalPost :: Config -> PostDB -> [Gopher.GopherMenuItem]
menuRepresentOriginalPost config post =
    let
        threadHeading = menuBuildHeadingParticular (config.menuViews.threadOpThreadNumberLabel <> (pack . show $ post.postId)) config.menuViews.threadOpHeadingDecorLeft config.menuViews.threadOpHeadingDecorRight False False
        threadMessage = messagePresenter config post.message
        wasBanned = wasBannedHelper config.language post.bannedForPost
    in
        toGopherMenuItems $ threadHeading ++ threadMessage ++ wasBanned

{- | A menu representation of a `PostDB` reply for the thread view.

-}
menuRepresentThreadReply :: Config -> PostDB -> [Gopher.GopherMenuItem]
menuRepresentThreadReply config post =
    let
        replyHeading = menuBuildHeadingParticular (config.menuViews.threadReplyNumberLabel <> (pack . show $ post.postId)) config.menuViews.threadReplyHeadingDecorLeft "" False False
        replyMessage = messagePresenter config post.message
    in
        toGopherMenuItems $ replyHeading ++ replyMessage

{- | A menu representation of a `PostDB` reply for the index view.

-}
menuRepresentIndexReply :: Config -> PostDB -> [Gopher.GopherMenuItem]
menuRepresentIndexReply config post =
    let
        replyMeta = "No. " <> (pack . show $ post.postId) <> ", " <> formatUTCTime config post.createdAt
        replyHeading = menuBuildHeadingParticular replyMeta config.menuViews.indexReplyHeadingDecorLeft config.menuViews.indexReplyHeadingDecorRight config.menuViews.indexReplyLeadingBreak config.menuViews.indexReplyTrailingBreak
        replyMessage = messagePresenter config post.message
    in
        toGopherMenuItems $ replyHeading ++ replyMessage

{- | Menu view for a thread, displaying the original post and all of its replies.

-}
menuViewThread :: Config -> PostDB -> [PostDB] -> [Gopher.GopherMenuItem]
menuViewThread config post replies =
    let
        originalPostLines = menuRepresentOriginalPost config post
        repliesLines = concatMap (menuRepresentThreadReply config) replies
        -- fromJust here bad?! this kind of sucks very bad type handling or whatever
        createReply = toGopherMenuItems $ createReplyLink config post.postId
        returnToIndex = toGopherMenuItems [menuBuildMenuLine config.language.returnToIndex "/"]
        viewThreadFileLink = toGopherMenuItems [menuBuildFileLine config.language.viewAsFile ("/" <> (pack . show $ post.postId) <> "/file")]
    in
        originalPostLines ++ repliesLines ++ createReply ++ viewThreadFileLink ++ returnToIndex

{- | Menu view for the thread index, displaying each thread and the latest three replies
for each, along with how many replies were omitted.

-}
menuViewIndex :: Config -> [PostDB] -> IO [Gopher.GopherMenuItem]
menuViewIndex config posts = do
    summaries <- concatMapM (menuRepresentIndexThreadSummary config) posts
    let
        preamble = toGopherMenuItems $ menuBuildInfoLines config.menuViews.indexPreamble
        newThread = toGopherMenuItems [menuBuildQueryLine config.language.createThread "/newthread"]

    pure $ preamble ++ newThread ++ summaries ++ newThread

concatMapM f xs = concat <$> traverse f xs

{- | Basic template logic for the "replies omitted" message for the index.

-}
repliesOmitted :: Config -> Int -> [GopherLine]
repliesOmitted config omitted
    | omitted == 1 = menuBuildInfoLines config.language.singleReplyOmitted
    | omitted > 1 =  menuBuildInfoLines $ (pack . show $ omitted) <> config.language.pluralRepliesOmitted
    | otherwise = []

createReplyLink config replyToId = [menuBuildQueryLine config.language.threadReply ("/" <> (pack . show $ replyToId) <> "/reply")]


{- | A representation of a `PostDB` (post from the database, namely a thread) for the index view.

-}
menuRepresentIndexThreadSummary :: Config -> PostDB -> IO [Gopher.GopherMenuItem]
menuRepresentIndexThreadSummary config post = do
    replyCount <- countReplies config.databaseConnection post.postId
    latestReplies <- getThreeLatestReplies config.databaseConnection post.postId
    let
        threadMetaInfo = helperPostMeta config post
        threadHeading = menuBuildHeadingParticular threadMetaInfo config.menuViews.indexOpHeadingDecorLeft config.menuViews.indexOpHeadingDecorRight True False
        createReply = createReplyLink config post.postId
        viewThreadMenuLink = menuBuildMenuLine config.language.viewAsMenu ("/" <> (pack . show $ post.postId) <> "/menu")
        viewThreadFileLink = menuBuildFileLine config.language.viewAsFile ("/" <> (pack . show $ post.postId) <> "/file")
        threadMessage = messagePresenter config post.message
        totalRepliesStatus = repliesOmitted config $ replyCount - (length latestReplies)
        latestRepliesLines = concatMap (menuRepresentIndexReply config) latestReplies

    pure $ (toGopherMenuItems $ threadHeading ++ createReply ++ [viewThreadMenuLink, viewThreadFileLink] ++ threadMessage ++ totalRepliesStatus) ++ latestRepliesLines
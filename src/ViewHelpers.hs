{- | Helper functions that are agnostic/used by both MenuViews and TextViews.

-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module ViewHelpers where

import Config
import Database
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Text (Text, pack, unpack)
import Data.Time.Clock (UTCTime)

formatUTCTime :: Config -> UTCTime -> Text
formatUTCTime config time = pack . formatTime defaultTimeLocale (unpack config.general.dateFormat) $ time

helperPostMeta :: Config -> PostDB -> Text
helperPostMeta config post = (config.language.threadIndexOpNumberLabel <> (pack . show $ post.postId)) <> ", " <> formatUTCTime config post.createdAt
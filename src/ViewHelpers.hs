{- | Helper functions that are agnostic/used by both MenuViews and TextViews.

-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
module ViewHelpers where

import Config
import Database

helperPostMeta :: PostDB -> String
helperPostMeta post = (languageThreadIndexOpNumberLabel ++ show post.postId) ++ ", " ++ show post.createdAt
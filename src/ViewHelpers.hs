{- | Helper functions that are agnostic/used by both MenuViews and TextViews.

-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
module ViewHelpers where

import Config

helperPostMeta post = (languageThreadIndexOpNumberLabel ++ show post.postId) ++ ", " ++ show post.createdAt
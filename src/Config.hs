{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Customization options and possibly internationalization.

Please consult the example configuration file for details.
-}
module Config where

import Data.Text (Text)
import GHC.Generics (Generic)
import Toml
import Toml qualified

data SpacecookieConfig = SpacecookieConfig
    { name :: Text
    , port :: Int
    }
    deriving (Generic, Show, Eq, Toml.HasCodec)

spacecookieConfigCodec :: TomlCodec SpacecookieConfig
spacecookieConfigCodec = Toml.genericCodec

data DatabaseConnectionConfig = DatabaseConnectionConfig
    { connectHost :: Text
    , connectPort :: Int
    , connectUser :: Text
    , connectPassword :: Text
    , connectDatabase :: Text
    }
    deriving (Generic, Show, Eq, Toml.HasCodec)

databaseConnectionConfigCodec :: TomlCodec DatabaseConnectionConfig
databaseConnectionConfigCodec = Toml.genericCodec

data LanguageConfig = LanguageConfig
    { viewAsFile :: Text
    , viewAsMenu :: Text
    , userWasBannedForThisPost :: Text
    , postRateLimitExceeded :: Text
    , youWereBannedLabel :: Text
    , failedToInsertPost :: Text
    , deletedThreadWithId :: Text
    , ipUnbannedSuccess :: Text
    , postNotFoundOrIpMissing :: Text
    , ipBannedSuccessfully :: Text
    , threadIndexOpNumberLabel :: Text
    , singleReplyOmitted :: Text
    , pluralRepliesOmitted :: Text
    , createThread :: Text
    , threadReply :: Text
    , returnToIndex :: Text
    , postTooLong :: Text
    , postEmpty :: Text
    }
    deriving (Generic, Show, Eq)

languageConfigCodec :: TomlCodec LanguageConfig
languageConfigCodec = Toml.genericCodec

data GeneralConfig = GeneralConfig
    { maximumPostLength :: Int
    , rateLimitMinutesNewThread :: Int
    , rateLimitMinutesNewReply :: Int
    , maximumRepliesPerThread :: Integer
    , maximumThreads :: Integer
    , maxWidth :: Integer
    , dateFormat :: Text
    }
    deriving (Eq, Show, Generic)

generalConfigCodec :: TomlCodec GeneralConfig
generalConfigCodec = Toml.genericCodec

data FileViewsConfig = FileViewsConfig
    { threadOpBorderHorizontal :: Text
    , threadOpBorderVertical :: Text
    , threadOpBorderCorner :: Text
    , threadOpMinimumWidth :: Int
    , threadOpMaximumWidth :: Int
    , threadOpPadding :: Int
    , threadReplyBorderHorizontal :: Text
    , threadReplyBorderVertical :: Text
    , threadReplyBorderCorner :: Text
    , threadReplyMinimumWidth :: Int
    , threadReplyMaximumWidth :: Int
    , threadReplyPadding :: Int
    }
    deriving (Eq, Show, Generic)

fileViewsConfigCodec :: TomlCodec FileViewsConfig
fileViewsConfigCodec = Toml.genericCodec

data MenuViewsConfig = MenuViewsConfig
    { indexReplyLeadingBreak :: Bool
    , indexReplyTrailingBreak :: Bool
    , indexOpHeadingDecorLeft :: Text
    , indexOpHeadingDecorRight :: Text
    , ommittedRepliesThreshold :: Int
    , indexPreamble :: Text
    , indexReplyHeadingDecorLeft :: Text
    , indexReplyHeadingDecorRight :: Text
    , threadOpHeadingDecorLeft :: Text
    , threadOpHeadingDecorRight :: Text
    , threadOpThreadNumberLabel :: Text
    , threadReplyNumberLabel :: Text
    , threadReplyHeadingDecorLeft :: Text
    }
    deriving (Eq, Show, Generic)

menuViewsConfigCodec :: TomlCodec MenuViewsConfig
menuViewsConfigCodec = Toml.genericCodec

-- FIXME: implement CharWrapper codec
data Config = Config
    { databaseConnection :: DatabaseConnectionConfig
    , language :: LanguageConfig
    , general :: GeneralConfig
    , fileViews :: FileViewsConfig
    , menuViews :: MenuViewsConfig
    , spacecookie :: SpacecookieConfig
    }
    deriving (Generic, Show, Eq)

configCodec :: TomlCodec Config
configCodec =
    Config
        <$> Toml.table databaseConnectionConfigCodec "databaseConnection" .= databaseConnection
        <*> Toml.table languageConfigCodec "language" .= language
        <*> Toml.table generalConfigCodec "general" .= general
        <*> Toml.table fileViewsConfigCodec "fileViews" .= fileViews
        <*> Toml.table menuViewsConfigCodec "menuViews" .= menuViews
        <*> Toml.table spacecookieConfigCodec "spacecookie" .= spacecookie

getConfig :: IO Config
getConfig = Toml.decodeFile configCodec "config.toml"
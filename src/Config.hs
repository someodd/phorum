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
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map
import Data.Maybe

data DaemonConfig = DaemonConfig
    { name :: Text
    , port :: Int
    , runAsUser :: Text
    }
    deriving (Generic, Show, Eq, Toml.HasCodec)

daemonConfigCodec :: TomlCodec DaemonConfig
daemonConfigCodec = Toml.genericCodec

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
    , threadRateLimitExceeded :: Text
    , replyRateLimitExceeded :: Text
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
    , spamReplyError :: Text
    , sameThreadError :: Text
    }
    deriving (Generic, Show, Eq)

languageConfigCodec :: TomlCodec LanguageConfig
languageConfigCodec = Toml.genericCodec

data GeneralConfig = GeneralConfig
    { selectorPrefix :: Text
    , maximumPostLength :: Int
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

arbitraryTableCodec :: Toml.Key -> TomlCodec (HashMap Text Text)
arbitraryTableCodec key = Toml.dimap (Map.fromList . HM.toList) (HM.fromList . Map.toList) (Toml.tableMap Toml._KeyText Toml.text key)

data Config = Config
    { databaseConnection :: DatabaseConnectionConfig
    , language :: LanguageConfig
    , general :: GeneralConfig
    , fileViews :: FileViewsConfig
    , menuViews :: MenuViewsConfig
    , daemon :: DaemonConfig
    , specialCodes :: HashMap Text Text
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
        <*> Toml.table daemonConfigCodec "daemon" .= daemon
        <*> arbitraryTableCodec "specialCodes" .= specialCodes

-- FIXME: detect if installed or running in nix?
getConfig :: FilePath -> IO Config
getConfig configFilePath = Toml.decodeFile configCodec configFilePath

{- | Return the value of a special code if it is found in the given text.

-}
findSpecialCode :: HashMap Text Text -> Text -> Maybe Text
findSpecialCode specialCodes text = 
    let codes = HM.keys specialCodes
    in listToMaybe [val | code <- codes, ("<" <> code <> ">") `T.isInfixOf` text, let val = fromJust (HM.lookup code specialCodes)]
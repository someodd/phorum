{- | Tools for constructing Gopher menus.

-}

{-# LANGUAGE OverloadedStrings #-}
module MenuBuild where

import Network.Gopher hiding (GopherFileType (..), GopherMenuItem (..))
import Network.Gopher qualified as Gopher (GopherFileType (..), GopherMenuItem (..))
import Data.ByteString.Char8 qualified as BC
import Data.List (intercalate, isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Tuple (swap)
import Data.List (find)

toGopherMenuItems :: [GopherLine] -> [Gopher.GopherMenuItem]
toGopherMenuItems = map (\(GopherLine itemType menuText selector maybeServerName maybePort) -> Gopher.Item (toGopherFileType itemType) (encodeUtf8 menuText) (encodeUtf8 selector) (encodeUtf8 <$> maybeServerName) (read . T.unpack <$> maybePort))

toGopherFileType :: ItemType -> Gopher.GopherFileType
toGopherFileType itemType = case itemType of
  File -> Gopher.File
  Directory -> Gopher.Directory
  PhoneBookServer -> Gopher.PhoneBookServer
  Error -> Gopher.Error
  BinHexMacintoshFile -> Gopher.BinHexMacintoshFile
  DOSArchive -> Gopher.DOSArchive
  UnixUuencodedFile -> Gopher.UnixUuencodedFile
  IndexSearchServer -> Gopher.IndexSearchServer
  TelnetSession -> Gopher.TelnetSession
  BinaryFile -> Gopher.BinaryFile
  RedundantServer -> Gopher.RedundantServer
  Tn3270Session -> Gopher.Tn3270Session
  GifFile -> Gopher.GifFile
  ImageFile -> Gopher.ImageFile
  InfoLine -> Gopher.InfoLine
  Html -> Gopher.Html

data ItemType
  = File
  | Directory
  | PhoneBookServer
  | Error
  | BinHexMacintoshFile
  | DOSArchive
  | UnixUuencodedFile
  | IndexSearchServer
  | TelnetSession
  | BinaryFile
  | RedundantServer
  | Tn3270Session
  | GifFile
  | ImageFile
  | InfoLine
  | Html
  deriving (Eq, Ord, Enum, Bounded)

itemTypeList :: [(ItemType, String, [String])]
itemTypeList = 
  [ (File, "0", ["txt", "doc", "pdf"])
  , (Directory, "1", [])
  , (PhoneBookServer, "2", [])
  , (Error, "3", [])
  , (BinHexMacintoshFile, "4", ["hqx"])
  , (DOSArchive, "5", ["zip", "rar", "7z"])
  , (UnixUuencodedFile, "6", ["uu"])
  , (IndexSearchServer, "7", [])
  , (TelnetSession, "8", [])
  , (BinaryFile, "9", ["bin"])
  , (RedundantServer, "+", [])
  , (Tn3270Session, "T", [])
  , (GifFile, "g", ["gif"])
  , (ImageFile, "I", ["jpg", "jpeg", "png", "bmp"])
  , (InfoLine, "i", [])
  , (Html, "h", ["html", "htm"])
  ]

lookupItemType :: String -> Maybe ItemType
lookupItemType str = lookup str [(b, a) | (a, b, _) <- itemTypeList]

lookupString :: ItemType -> Maybe String
lookupString itemType = lookup itemType [(a, b) | (a, b, _) <- itemTypeList]

lookupExtension :: String -> Maybe ItemType
lookupExtension ext = fmap (\(a, _, _) -> a) . find ((ext `elem`) . thd) $ itemTypeList
  where
    thd (_, _, x) = x

instance Show ItemType where
  show item = case lookupString item of
    Just str -> str
    Nothing -> error "lookupString failed"

{- | A GopherMenuItem is a line in a gopher menu/directory.

I use this for building the dashboard.

File type, menu text, selector, server name (optional), port (optional).

None of the fields may use tabs.
-}
data GopherLine = GopherLine ItemType Text Text (Maybe Text) (Maybe Text) deriving (Show)

lineToText :: GopherLine -> Text
lineToText (GopherLine itemType menuText selector maybeServerName maybePort) =
    (T.pack $ show itemType) <> T.intercalate "\t" [menuText, selector, fromMaybe "" maybeServerName, fromMaybe "" maybePort]

menuBuildHeading :: Text -> GopherLine
menuBuildHeading title = GopherLine InfoLine ("### " <> title <> " ###") "" Nothing Nothing

menuBuildHeadingParticular :: Text -> Text -> Text -> Bool -> Bool -> [GopherLine]
menuBuildHeadingParticular title decorLeft decorRight leadingBreak trailingBreak = do
    (if leadingBreak then [menuBuildBlankLine] else []) <> [GopherLine InfoLine (decorLeft <> title <> decorRight) "" Nothing Nothing] ++ (if trailingBreak then [menuBuildBlankLine] else [])

menuBuildQueryLine :: Text -> Text -> GopherLine
menuBuildQueryLine string selector = GopherLine IndexSearchServer string selector Nothing Nothing

menuBuildMenuLine :: Text -> Text -> GopherLine
menuBuildMenuLine label selector = GopherLine Directory label selector Nothing Nothing

menuBuildFileLine :: Text -> Text -> GopherLine
menuBuildFileLine label selector = GopherLine File label selector Nothing Nothing

menuBuildBlankLine :: GopherLine
menuBuildBlankLine = GopherLine InfoLine "" "" Nothing Nothing

menuBuildSection :: Bool -> Text -> Maybe [GopherLine] -> [GopherLine] -> [GopherLine]
menuBuildSection leadingNewLine sectionName maybePreambleList menuItems =
  [menuBuildBlankLine | leadingNewLine]
    ++ [menuBuildHeading sectionName]
    ++ fromMaybe [] maybePreambleList
    ++ menuItems

menuBuildInfoLine :: Text -> GopherLine
menuBuildInfoLine string = GopherLine InfoLine string "" Nothing Nothing

menuBuildInfoLines :: Text -> [GopherLine]
menuBuildInfoLines = map menuBuildInfoLine . T.lines

menuBuildInfoLinesQuoted :: Text -> [GopherLine]
menuBuildInfoLinesQuoted = map menuBuildInfoLine . map (">" <>) . T.lines

data GopherMenu = GopherMenu [GopherLine]

menuToText :: GopherMenu -> Text
menuToText (GopherMenu lines) = T.intercalate "\n" $ map lineToText lines


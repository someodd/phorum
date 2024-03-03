{- | Tools for constructing Gopher menus.

-}

module MenuBuild where

import Network.Gopher hiding (GopherFileType (..), GopherMenuItem (..))
import Network.Gopher qualified as Gopher (GopherFileType (..), GopherMenuItem (..))
import Data.ByteString.Char8 qualified as BC
import Data.List (intercalate, isPrefixOf)
import Data.Maybe (fromMaybe)

toGopherMenuItems :: [GopherLine] -> [Gopher.GopherMenuItem]
toGopherMenuItems = map (\(GopherLine itemType menuText selector maybeServerName maybePort) -> Gopher.Item (toGopherFileType itemType) (BC.pack menuText) (BC.pack selector) (BC.pack <$> maybeServerName) (read <$> maybePort))

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
  deriving (Eq, Ord, Enum)

instance Show ItemType where
  show item = case item of
    File -> "0"
    Directory -> "1"
    PhoneBookServer -> "2"
    Error -> "3"
    BinHexMacintoshFile -> "4"
    DOSArchive -> "5"
    UnixUuencodedFile -> "6"
    IndexSearchServer -> "7"
    TelnetSession -> "8"
    BinaryFile -> "9"
    RedundantServer -> "+"
    Tn3270Session -> "T"
    GifFile -> "g"
    ImageFile -> "I"
    InfoLine -> "i"
    Html -> "h"

{- | A GopherMenuItem is a line in a gopher menu/directory.

I use this for building the dashboard.

File type, menu text, selector, server name (optional), port (optional).

None of the fields may use tabs.
-}
data GopherLine = GopherLine ItemType String String (Maybe String) (Maybe String)

instance Show GopherLine where
  show (GopherLine itemType menuText selector maybeServerName maybePort) =
    show itemType ++ intercalate "\t" [menuText, selector, fromMaybe "" maybeServerName, fromMaybe "" maybePort]

menuBuildHeading :: String -> GopherLine
menuBuildHeading title = GopherLine InfoLine ("### " ++ title ++ " ###") "" Nothing Nothing

menuBuildHeadingParticular :: String -> String -> String -> Bool -> Bool -> [GopherLine]
menuBuildHeadingParticular title decorLeft decorRight leadingBreak trailingBreak = do
    (if leadingBreak then [menuBuildBlankLine] else []) ++ [GopherLine InfoLine (decorLeft ++ title ++ decorRight) "" Nothing Nothing] ++ (if trailingBreak then [menuBuildBlankLine] else [])

menuBuildQueryLine :: String -> String -> GopherLine
menuBuildQueryLine string selector = GopherLine IndexSearchServer string selector Nothing Nothing

menuBuildMenuLine :: String -> String -> GopherLine
menuBuildMenuLine label selector = GopherLine Directory label selector Nothing Nothing

menuBuildFileLine :: String -> String -> GopherLine
menuBuildFileLine label selector = GopherLine File label selector Nothing Nothing

menuBuildBlankLine :: GopherLine
menuBuildBlankLine = GopherLine InfoLine "" "" Nothing Nothing

menuBuildSection :: Bool -> String -> Maybe [GopherLine] -> [GopherLine] -> [GopherLine]
menuBuildSection leadingNewLine sectionName maybePreambleList menuItems =
  [menuBuildBlankLine | leadingNewLine]
    ++ [menuBuildHeading sectionName]
    ++ fromMaybe [] maybePreambleList
    ++ menuItems

menuBuildInfoLine :: String -> GopherLine
menuBuildInfoLine string = GopherLine InfoLine string "" Nothing Nothing

menuBuildInfoLines :: String -> [GopherLine]
menuBuildInfoLines = map menuBuildInfoLine . lines

menuBuildInfoLinesQuoted :: String -> [GopherLine]
menuBuildInfoLinesQuoted = map menuBuildInfoLine . map (">" ++) . lines

data GopherMenu = GopherMenu [GopherLine]

instance Show GopherMenu where
  show (GopherMenu lines) = intercalate "\n" $ map show lines


{-# LANGUAGE OverloadedStrings #-}

{- | Make gopher links out of Gopher and HTTP URIs.

-}
module LinkDetection where

import MenuBuild

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Char (toLower)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Network.URI (parseURI, uriAuthority, uriPath, uriPort, uriRegName, uriScheme)
import System.FilePath (takeExtension)
import System.FilePath.Posix (takeFileName)

{- | Detect if a given Text is a gopher URI, if so we want to transform it into a gopher
menu link.

The entirety of text must be a link, otherwise Nothing is returned.

Detects the `ItemType` by first looking at the first character of the path, then looking
at the file extension.

= Examples

>>> parseGopherURI "gopher://example.com/"
Just (GopherLine {gopherType = Menu, gopherDisplay = "example.com", gopherSelector = "1/somemenu", gopherHost = "example.com", gopherPort = 70})
>>> parseGopherURI "gopher://gopher.example.org:7070/0/book.txt"
Just (GopherLine {gopherType = Text, gopherDisplay = "book.txt (gopher.example.org)", gopherSelector = "0/book.txt", gopherHost = "gopher.example.org", gopherPort = 7070})
>>> parseGopherURI "gopher://gopher.example.org:7070/0/book.txt test"
Nothing
>>> parseGopherURI "something else"
Nothing
>>> parseGopherURI "gopher.example.org:7070/0/book.txt"
Nothing
-}
parseGopherURI :: T.Text -> Maybe GopherLine
parseGopherURI text = do
    uri <- parseURI (T.unpack text)
    guard (uriScheme uri == "gopher:")
    auth <- uriAuthority uri
    let path = uriPath uri
        serverName = T.pack $ uriRegName auth
        port = if null (uriPort auth) then Nothing else Just (T.pack $ tail $ uriPort auth) -- tail to remove leading ':'
        ext = map toLower $ drop 1 $ takeExtension path -- drop 1 to remove leading '.'
        itemTypeFromPath = case splitOn "/" path of
            (_ : itemTypeStr : _) -> lookupItemType itemTypeStr
            _ -> Nothing
        itemTypeFromExt = lookupExtension ext
        (itemType :: ItemType) = fromMaybe Directory (itemTypeFromPath <|> itemTypeFromExt)
        selector = T.pack path
        display = case reverse (splitOn "/" path) of
            (filename : _) | not (null filename) -> T.pack filename <> " (" <> serverName <> ")"
            _ -> if null path || path == "/" then serverName else T.pack $ last $ splitOn "/" path
    return $ GopherLine itemType display selector (Just serverName) port

{- | Detect if a given Text is an HTTP URI, if so we want to transform it into a gopher
html link.

If it's an actual web link the ItemType is always Html.

The entirety of text must be a link, otherwise Nothing is returned.

= Examples

>>> parseHttpURI "http://example.com/"
Just (GopherLine {gopherType = Html, gopherDisplay = "example.com", gopherSelector = "http://example.com/", gopherHost = "example.com", gopherPort = Nothing})
>>> parseHttpURI "http://example.com/something/whatever.zip"
Just (GopherLine {gopherType = Html, gopherDisplay = "whatever.zip (example.com)", gopherSelector = "http://example.com/something/whatever.zip", gopherHost = "example.com", gopherPort = Nothi
-}
parseHttpURI :: T.Text -> Maybe GopherLine
parseHttpURI text = do
    uri <- parseURI (T.unpack text)
    guard (uriScheme uri == "https:" || uriScheme uri == "http")
    auth <- uriAuthority uri
    let path = uriPath uri
        serverName = T.pack $ uriRegName auth
        filename = T.pack $ takeFileName path
        display = if T.null filename then serverName else filename <> " (" <> serverName <> ")"
    return $ GopherLine Html display (T.pack $ show uri) (Just serverName) Nothing

{- | Detect if a given Text is a link (http or gopher), if so we want to transform it into
a link for a gopher menu.

If it's not parsed as a link we just give back the original text.
-}
parseLink :: T.Text -> [GopherLine]
parseLink text = fromMaybe (menuBuildInfoLines text) ((: []) <$> (parseGopherURI text <|> parseHttpURI text))
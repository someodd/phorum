{- | Basically tools for building the ASCII art representations of things for a text file.

-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module TextBuild where

import Data.Text (Text, lines, unlines, length, replicate, pack)
import Prelude hiding (lines, unlines, length, replicate)

{- | Create a fancy ASCII-art style text box for content.

Features:
  - Centered header (within the top border)
  - Body
  - ASCII art box; contents are enclosed in a border
  - Configurable padding
  - fileMinimumWidth
  - fileMaximumWidth
  - Configurable characters for the horizontal, vertical, and corner border components

-}
textBuildBox :: Text -> Text -> Char -> Char -> Char -> Int -> Int -> Int -> Text
textBuildBox header body horizontalBorder verticalBorder cornerDecor fileMinimumWidth fileMaximumWidth padding =
  let bodyLines = lines body
      initialBoxWidth = maximum (map length bodyLines) + (padding * 2) + 2 -- +2 for the vertical borders
      boxWidth = max initialBoxWidth fileMinimumWidth
      finalBoxWidth = min boxWidth fileMaximumWidth
      headerSpace = finalBoxWidth - length header - 2 -- -2 for the corner decorations
      leftHeaderPadding = (headerSpace `div` 2)
      rightHeaderPadding = headerSpace - leftHeaderPadding
      topBorder = pack [cornerDecor] <> replicate leftHeaderPadding (pack [horizontalBorder]) <> header <> replicate rightHeaderPadding (pack [horizontalBorder]) <> pack [cornerDecor]
      bottomBorder = pack [cornerDecor] <> replicate (finalBoxWidth - 2) (pack [horizontalBorder]) <> pack [cornerDecor]
      paddedBody = map (\line -> pack [verticalBorder] <> " " <> line <> replicate (finalBoxWidth - length line - 4) " " <> pack [' ', verticalBorder]) bodyLines
      emptyLine = pack [verticalBorder] <> replicate (finalBoxWidth - 2) " " <> pack [verticalBorder]
  in unlines (topBorder : emptyLine : paddedBody ++ [emptyLine, bottomBorder])
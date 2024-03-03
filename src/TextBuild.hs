{- | Basically tools for building the ASCII art representations of things for a text file.

-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
module TextBuild where

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
textBuildBox :: String -> String -> Char -> Char -> Char -> Int -> Int -> Int -> String
textBuildBox header body horizontalBorder verticalBorder cornerDecor fileMinimumWidth fileMaximumWidth padding =
  let bodyLines = lines body
      initialBoxWidth = maximum (map length bodyLines) + (padding * 2) + 2 -- +2 for the vertical borders
      boxWidth = max initialBoxWidth fileMinimumWidth
      finalBoxWidth = min boxWidth fileMaximumWidth
      headerSpace = finalBoxWidth - length header - 2 -- -2 for the corner decorations
      leftHeaderPadding = (headerSpace `div` 2)
      rightHeaderPadding = headerSpace - leftHeaderPadding
      topBorder = cornerDecor : replicate leftHeaderPadding horizontalBorder ++ header ++ replicate rightHeaderPadding horizontalBorder ++ [cornerDecor]
      bottomBorder = [cornerDecor] ++ replicate (finalBoxWidth - 2) horizontalBorder ++ [cornerDecor]
      paddedBody = map (\line -> verticalBorder : ' ' : line ++ replicate (finalBoxWidth - length line - 4) ' ' ++ [' ', verticalBorder]) bodyLines
      emptyLine = [verticalBorder] ++ replicate (finalBoxWidth - 2) ' ' ++ [verticalBorder]
  in unlines (topBorder : emptyLine : paddedBody ++ [emptyLine, bottomBorder])


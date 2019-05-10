module TextLayout where

import Data.List (intercalate)

-- | Draw a grid of strings.
grid :: [[String]] -> String
grid table = concatMap renderRow table
  where
    renderRow row = "| " ++ intercalate " | " (paddedRow row) ++ " |\n"
    nCols = maximum (map length table)

    columnWidths :: [Int]
    columnWidths = take nCols (foldr merge (repeat 0) table) where
      merge row = zipWith max (map length row ++ repeat 0)

    paddedRow row
      = zipWith padTo columnWidths (row ++ replicate (nCols - length row) "")
    padTo n cell = cell ++ replicate (n - length cell) ' '

module Validator(isValidMove) where

import Grid (Grid)

isValidMove :: Grid -> Int -> Int -> Int -> Bool
isValidMove grid row col num = 
  grid !! row !! col  == 0 &&
  isValidInRow num (grid !! row) &&
  isValidInCol num grid col &&
  isValidInBox num grid (row, col)
  where
    isValidInRow n row = notElem n row
    isValidInCol n grid col = notElem n [grid !! r !! col | r <- [0..8]]
    isValidInBox n grid (row, col) = 
      let startRow = (row `div` 3) * 3
          startCol = (col `div` 3) * 3
          box = [grid !! r !! c | r <- [startRow..startRow + 2], c <- [startCol..startCol + 2]]
      in notElem n box
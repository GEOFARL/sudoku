module Validator(isValidMove, isValidGrid) where

import Grid (Grid)
import Data.List (nub, transpose)

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

isValidUnit :: [Int] -> Bool
isValidUnit xs = let nums = filter (/= 0) xs
                  in length nums == length (nub nums)

areRowsValid :: Grid -> Bool 
areRowsValid = all isValidUnit

areColsValid :: Grid -> Bool
areColsValid grid = areRowsValid (transpose grid)

areSubgridsValid :: Grid -> Bool
areSubgridsValid grid = all isValidUnit [extractBox r c grid | r <- [0,3,6], c <- [0,3,6]]
  where
    extractBox r c grid = [grid !! (r + dr) !! (c + dc) | dr <- [0..2], dc <- [0..2]]

isValidGrid :: Grid -> Bool
isValidGrid grid = areRowsValid grid && areColsValid grid && areSubgridsValid grid
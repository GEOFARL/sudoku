module Grid(Grid, printGrid, sampleGrid, updateGrid, isGridFilled, almostSolvedGrid) where

import Control.Monad (when)

type Row = [Int]
type Grid = [Row]

sampleGrid :: Grid
sampleGrid =
  [ [5, 3, 0, 0, 7, 0, 0, 0, 0]
  , [6, 0, 0, 1, 9, 5, 0, 0, 0]
  , [0, 9, 8, 0, 0, 0, 0, 6, 0]
  , [8, 0, 0, 0, 6, 0, 0, 0, 3]
  , [4, 0, 0, 8, 0, 3, 0, 0, 1]
  , [7, 0, 0, 0, 2, 0, 0, 0, 6]
  , [0, 6, 0, 0, 0, 0, 2, 8, 0]
  , [0, 0, 0, 4, 1, 9, 0, 0, 5]
  , [0, 0, 0, 0, 8, 0, 0, 7, 9]
  ]

almostSolvedGrid :: [[Int]]
almostSolvedGrid =
  [ [1, 2, 3, 4, 6, 8, 7, 5, 9]
  , [9, 4, 5, 2, 7, 3, 1, 6, 8]
  , [6, 7, 8, 5, 1, 9, 2, 3, 4]
  , [2, 1, 4, 3, 5, 7, 8, 9, 6]
  , [5, 8, 7, 6, 9, 4, 3, 1, 2]
  , [3, 6, 9, 1, 8, 2, 4, 7, 5]
  , [4, 3, 1, 9, 2, 5, 6, 8, 7]
  , [7, 5, 6, 0, 4, 1, 9, 2, 3]  -- One missing number here (should be 8)
  , [8, 9, 2, 7, 3, 6, 5, 4, 1]
  ]

emptyGrid :: Grid
emptyGrid = replicate 9 (replicate 9 0)

printGrid :: Grid -> IO ()
printGrid grid = do
  putStrLn horizontalSeparator
  mapM_ printRow (zip grid [1..])
  where
    horizontalSeparator = "+-------+-------+-------+"

    printRow (row, i) = do
      putStrLn $ "| " ++ unwords (addVerticalSeparators (map showOrEmpty row)) ++ " |"
      when (i `mod` 3 == 0) $ putStrLn horizontalSeparator

    addVerticalSeparators :: [String] -> [String]
    addVerticalSeparators row = 
      let (block1, rest1) = splitAt 3 row
          (block2, block3) = splitAt 3 rest1
      in block1 ++ ["|"] ++ block2 ++ ["|"] ++ block3

    showOrEmpty :: Int -> String
    showOrEmpty 0 = "."
    showOrEmpty n = show n

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

updateGrid :: Grid -> Int -> Int -> Int -> Grid
updateGrid grid row col num =
  take row grid ++
  [take col (grid !! row) ++ [num] ++ drop (col + 1) (grid !! row)] ++
  drop (row + 1) grid

isGridFilled :: Grid -> Bool
isGridFilled grid = all (all (/= 0)) grid
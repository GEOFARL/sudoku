module Grid(Grid, printGrid, sampleGrid, updateGrid) where

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

emptyGrid :: Grid
emptyGrid = replicate 9 (replicate 9 0)

printGrid :: Grid -> IO ()
printGrid grid = mapM_ print grid

updateGrid :: Grid -> Int -> Int -> Int -> Grid
updateGrid grid row col num =
  take row grid ++
  [take col (grid !! row) ++ [num] ++ drop (col + 1) (grid !! row)] ++
  drop (row + 1) grid
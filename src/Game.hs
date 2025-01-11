module Game (gameLoop) where
import Grid (printGrid, Grid, updateGrid, isGridFilled)
import Validator (isValidMove, isValidGrid)

gameLoop :: Grid -> IO ()
gameLoop grid = do
  printGrid grid
  if isGameComplete grid
    then putStrLn "Congratulations! You solved the puzzle."
    else do
      (row, col, num) <- readMove
      if isValidMove grid row col num
        then do
          let newGrid = updateGrid grid row col num
          gameLoop newGrid
        else do
          putStrLn "Invalid move! Try again."
          gameLoop grid

readMove :: IO (Int, Int, Int)
readMove = do
  putStrLn "Enter your move (row column number):"
  line <- getLine
  let [row, col, num] = map read $ words line
  return (row - 1, col - 1, num)

isGameComplete :: Grid -> Bool
isGameComplete grid = isGridFilled grid && isValidGrid grid
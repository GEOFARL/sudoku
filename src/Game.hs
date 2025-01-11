module Game (gameLoop) where
import Grid (printGrid, Grid, updateGrid)
import Validator (isValidMove)

gameLoop :: Grid -> IO ()
gameLoop grid = do
  printGrid grid
  (row, col, num) <- readMove
  if isValidMove grid row col num
    then do
      let newGrid = updateGrid grid row col num
      gameLoop newGrid
    else do
      putStrLn "Invalid move. Try again."
      gameLoop grid

readMove :: IO (Int, Int, Int)
readMove = do
  putStrLn "Enter your move (row column number):"
  line <- getLine
  let [row, col, num] = map read $ words line
  return (row - 1, col - 1, num)
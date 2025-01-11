module Game (gameLoop) where
import Grid (printGrid, Grid, updateGrid, isGridFilled)
import Validator (isValidMove, isValidGrid)
import Data.Char (isDigit)

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
  putStrLn "Enter your move (row column number) or 'q' to quit:"
  input <- getLine
  if input == "q"
    then do
      putStrLn "Thanks for playing!"
      error "Game terminated by user"
    else
      case words input of
        [rowStr, colStr, numStr]
          | all isDigit rowStr && all isDigit colStr && all isDigit numStr ->
              return (read rowStr - 1, read colStr - 1, read numStr - 1)
        _ -> do
          putStrLn "Invalid input format, please try"
          readMove

isGameComplete :: Grid -> Bool
isGameComplete grid = isGridFilled grid && isValidGrid grid
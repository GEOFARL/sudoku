module Main where

import Grid (sampleGrid, almostSolvedGrid)
import Game (gameLoop)

main :: IO ()
main = do
  putStrLn "Welcome to Sudoku!"
  gameLoop almostSolvedGrid

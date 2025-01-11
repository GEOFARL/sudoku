module Main where

import Grid (sampleGrid)
import Game (gameLoop)

main :: IO ()
main = do
  putStrLn "Welcome to Sudoku!"
  gameLoop sampleGrid

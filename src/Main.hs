module Main (main) where
import Grid (printGrid, sampleGrid, updateGrid)
import Validator (isValidMove)

main :: IO ()
main = do
  printGrid sampleGrid

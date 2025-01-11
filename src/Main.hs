module Main (main) where
import Grid (printGrid, sampleGrid)
import Validator (isValidMove)

main :: IO ()
main = do
  printGrid sampleGrid

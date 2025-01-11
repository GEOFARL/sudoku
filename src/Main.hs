module Main (main) where
import Grid (printGrid, sampleGrid)

main :: IO ()
main = do
  printGrid sampleGrid

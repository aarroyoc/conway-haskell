module Reader (readVacaFile) where

import System.IO
import qualified Data.Matrix as Matrix

readVacaFile :: IO (Matrix.Matrix Int)
readVacaFile = do
    file <- readFile "gosper.txt"
    let alto = read ((lines file) !! 0) :: Int
    let ancho = read ((lines file) !! 1) :: Int
    let m = drop 2 (lines file)
    let matrix = buildMatrix alto ancho m
    return matrix

buildMatrix :: Int -> Int -> [String] -> Matrix.Matrix Int
buildMatrix alto ancho m = Matrix.matrix alto ancho builder 
    where
        builder (i,j) = if ((m !! (i-1)) !! (j-1) == 'X') then 1 else 0

module Main where

import Data.Matrix
import qualified Data.Vector as Vector
import qualified Reader
import qualified Data.Maybe as Maybe

main :: IO ()
main = do
    matrix <- Reader.readVacaFile
    putStrLn "Iterar cuantas veces?"
    n <- getLine
    let times = read n :: Int
    let finalMatrix = Prelude.iterate Main.iterate matrix
    putStrLn $ prettyMatrix $ finalMatrix !! times


iterate :: Matrix Int -> Matrix Int
iterate m = 
    if hasToGrow then
        matrix (nrows m +2) (ncols m +2) (\(i,j) -> builder (i-1,j-1))
    else
        matrix (nrows m) (ncols m) builder
    where
        builder (i,j) = 
            if get (i,j) == 0 then
                if hasToBorn (i,j) then
                    1
                else
                    0
            else
                if hasToDie (i,j) then
                    0
                else
                    1
        hasToGrow = 
            Vector.sum (getCol (ncols m) m) > 0 || 
            Vector.sum (getRow (nrows m) m) > 0 ||
            Vector.sum (getCol 1 m) > 0 ||
            Vector.sum (getRow 1 m) > 0
        get (i,j) = Maybe.fromMaybe 0 (safeGet i j m)
        hasToBorn (i,j) = sumNeighbors (i,j) == 3
        hasToDie (i,j) = sumNeighbors (i,j) /= 2 && sumNeighbors (i,j) /= 3
        sumNeighbors (i,j) = 
            get (i-1,j-1) + get (i,j-1) + get (i+1,j-1) 
            + get (i-1,j) + get (i+1,j)
            + get (i-1,j+1) + get (i,j+1) + get (i+1,j+1)


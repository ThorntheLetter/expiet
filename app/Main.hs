module Main where

import System.Environment
import Data.Array

import Image
import Flood


main :: IO ()
main = do
    args <- getArgs
    arr <- parseFile Nothing (head args)
    (blarr, blocks) <- return (floodFillAll arr)
    printArr blarr
    print blocks

-- not fancy butt it works well enough
printArr :: Array (Int, Int) Int -> IO ()
printArr arr = do
    ((_, _), (mx, my)) <- return (bounds arr)
    sequence_ [print [arr ! (x,y) | x <- [0..mx]] | y <- [0..my]]

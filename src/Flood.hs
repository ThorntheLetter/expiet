{-# LANGUAGE TupleSections #-}
module Flood where

import Data.Array

import Debug.Trace

type FFcase = (Array (Int, Int) Int, Array (Int, Int) Int, Int)

floodFillAll :: Array (Int, Int) Int -> (Array (Int, Int) Int, Int)
floodFillAll carr = (narr, nblocks)
    where
        is = indices carr
        base = (carr, array (bounds carr) (map (, -1) is), 0)
        (_, narr, nblocks) = foldl floodFill base is

floodFill :: FFcase -> (Int, Int) -> FFcase 
floodFill (carr, narr, nblocks) i =
    case narr ! i of
        -1 -> floodFill' (carr, narr, nblocks + 1) i (carr ! i)
        _ -> (carr, narr, nblocks)

floodFill' :: FFcase -> (Int, Int) -> Int -> FFcase
floodFill' c i v =
    if inBounds && (carr ! i) == v && (narr ! i) == -1 
    then c'''''
    else c
    where
        (carr, narr, nblocks) = c
        (x, y) = i
        ((minx, miny), (maxx, maxy)) = bounds carr
        bnumber = nblocks - 1
        within i min max = i >= min && i <= max
        inBounds = within x minx maxx && within y miny maxy
        c' = (carr, narr // [(i, bnumber)], nblocks)
        c'' = floodFill' c' (x + 1, y) v -- a fold would probably be better
        c''' = floodFill' c'' (x - 1, y) v
        c'''' = floodFill' c''' (x, y + 1) v
        c''''' = floodFill' c'''' (x, y - 1) v

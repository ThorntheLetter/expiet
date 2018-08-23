{-# LANGUAGE TupleSections #-}
module Flood where

import Data.Array

type BlockInfo = (Int, Array Int (Int, Int)) -- (size, border coords)
type FFsummary = (FFcase, [BlockInfo])
type FFcase = (Array (Int, Int) Int, Array (Int, Int) Int, Int)

floodFillAll :: Array (Int, Int) Int -> (Array (Int, Int) Int, [BlockInfo])
floodFillAll carr = (narr, blocks)
    where
        is = indices carr
        base = ((carr, array (bounds carr) (map (, -1::Int) is), 0::Int), [])
        ((_, narr, nblocks), blocks) = foldl floodFill base is

floodFill :: FFsummary -> (Int, Int) -> FFsummary
floodFill iminfo i =
    case narr ! i of
        -1 -> ffpToSum iminfo (floodFill' (carr ! i) ((carr, narr, nblocks + 1), baseBlock) i)
        _ -> iminfo
    where
        ((carr, narr, nblocks), blocksInfo) = iminfo
        baseBlock = (0, array (0, 7) [(x, i) | x <- [0..7]])
        ffpToSum (c, bs) (cc, b) = (cc, bs ++ [b])
        

floodFill' :: Int -> (FFcase, BlockInfo) -> (Int, Int) -> (FFcase, BlockInfo)
floodFill' v c i =
    if inBounds && (carr ! i) == v && (narr ! i) == -1 
    then foldl (floodFill' v) c' neighbors
    else c
    where
        ((carr, narr, nblocks), bi) = c
        (x, y) = i
        ((minx, miny), (maxx, maxy)) = bounds carr
        bnumber = nblocks - 1
        within i min max = i >= min && i <= max
        inBounds = within x minx maxx && within y miny maxy
        neighbors = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
        c' = ((carr, narr // [(i, bnumber)], nblocks), updateBlock bi i)

updateBlock :: BlockInfo -> (Int, Int) -> BlockInfo
updateBlock (size, borders) point = (size + 1, newBorders)
    where
        bfunctions = map mkBlockFn [(2, 1), (2, -1), (1, -2), (-1, -2), (-2, -1), (-2, 1), (-1, 2), (1, 2)] 
        newBorders = array (0, 7) [(i, updateBorder i) | i <- [0..7]]
        updateBorder i = (bfunctions !! i) (borders ! i) point

mkBlockFn :: (Int, Int) -> ((Int, Int) -> (Int, Int) -> (Int, Int))
mkBlockFn (wx, wy)
    | wx == 2 && wy == 1 = \(x,y) (x', y') -> if x' > x || (x' == x && y' < y) then (x', y') else (x, y)
    | wx == 2 && wy == -1 = \(x,y) (x', y') -> if x' > x || (x' == x && y' > y) then (x', y') else (x, y)
    | wx == -2 && wy == 1 = \(x,y) (x', y') -> if x' < x || (x' == x && y' < y) then (x', y') else (x, y)
    | wx == -2 && wy == -1 = \(x,y) (x', y') -> if x' < x || (x' == x && y' > y) then (x', y') else (x, y)
    | wx == 1 && wy == 2 = \(x,y) (x', y') -> if y' < y || (y' == y && x' > x) then (x', y') else (x, y)
    | wx == 1 && wy == -2 = \(x,y) (x', y') -> if y' > y || (y' == y && x' > x) then (x', y') else (x, y)
    | wx == -1 && wy == 2 = \(x,y) (x', y') -> if y' < y || (y' == y && x' < x) then (x', y') else (x, y)
    | wx == -1 && wy == -2 = \(x,y) (x', y') -> if y' > y || (y' == y && x' < x) then (x', y') else (x, y)
            

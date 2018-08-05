module Image where

import Codec.Picture
import Data.Array
import System.Exit

import Data.Typeable

parseFile :: Maybe (PixelRGBA8 -> Int) -> FilePath -> IO (Array (Int, Int) Int)
--parseFile :: Maybe (PixelRGBA8 -> Int) -> FilePath -> IO ()
parseFile Nothing file = parseFileWithPalette vanillaPalette file
parseFile (Just palette) file = parseFileWithPalette palette file

vanillaPalette :: PixelRGBA8 -> Int
vanillaPalette p = case p of
    PixelRGBA8 0x00 0x00 0x00 0xff -> -1 -- black
    PixelRGBA8 0xff 0xc0 0xc0 0xff -> 0  -- reds
    PixelRGBA8 0xff 0x00 0x00 0xff -> 1
    PixelRGBA8 0xc0 0x00 0x00 0xff -> 2
    PixelRGBA8 0xff 0xff 0xc0 0xff -> 3  -- yellows
    PixelRGBA8 0xff 0xff 0x00 0xff -> 4
    PixelRGBA8 0xc0 0xc0 0x00 0xff -> 5
    PixelRGBA8 0xc0 0xff 0xc0 0xff -> 6  -- greens
    PixelRGBA8 0x00 0xff 0x00 0xff -> 7
    PixelRGBA8 0x00 0xc0 0x00 0xff -> 8
    PixelRGBA8 0xc0 0xff 0xff 0xff -> 9  -- cyans
    PixelRGBA8 0x00 0xff 0xff 0xff -> 10
    PixelRGBA8 0x00 0xc0 0xc0 0xff -> 11
    PixelRGBA8 0xc0 0xc0 0xff 0xff -> 12 -- blues
    PixelRGBA8 0x00 0x00 0xff 0xff -> 13
    PixelRGBA8 0x00 0x00 0xc0 0xff -> 14
    PixelRGBA8 0xff 0xc0 0xff 0xff -> 15 -- magentas
    PixelRGBA8 0xff 0x00 0xff 0xff -> 16
    PixelRGBA8 0xc0 0x00 0xc0 0xff -> 17
    _ -> -2 -- white / others

parseFileWithPalette :: (PixelRGBA8 -> Int) -> FilePath -> IO (Array (Int, Int) Int)
-- parseFileWithPalette :: (PixelRGBA8 -> Int) -> FilePath -> IO ()
parseFileWithPalette palette file = do
    e <- readImage file
    case e of
        Left err -> die err
        Right img ->  return (mapImageToArray palette (convertRGBA8 img))
        --Right img -> print file

mapImageToArray :: (PixelRGBA8 -> Int) -> Image PixelRGBA8 -> Array (Int, Int) Int
mapImageToArray f i = array ((0, 0), (mx, my)) [((x, y), f (pixelAt i x y)) | x <- [0..mx], y <- [0..my]] 
    where
        mx = imageWidth i - 1
        my = imageHeight i - 1
 

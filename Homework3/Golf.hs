{-# OPTIONS_GHC -Wall #-}

module Golf where

skips :: [a] -> [[a]]
skips xs = map (`skip` xs) [0 .. length xs - 1]
  where
    skip :: Int -> [a] -> [a]
    skip _ [] = []
    skip 0 ys = ys
    skip n ys =
        case drop n ys of
            []     -> []
            (x:zs) -> x : skip n zs

localMaxima :: [Integer] -> [Integer]
localMaxima (x:xs@(y:z:_))
    | y > x && y > z = y : localMaxima xs
    | otherwise = localMaxima xs
localMaxima _ = []

histogram :: [Integer] -> String
histogram xs = bars <> bottom
  where
    bars :: String
    bars = unlines $ map drawRow [height,height - 1 .. 1]
    drawRow :: Int -> String
    drawRow row = map (draw row) counts
    draw :: Int -> Int -> Char
    draw row freq
        | freq >= row = '*'
        | otherwise = ' '
    height :: Int
    height = maximum $ map count [0 .. 9]
    counts :: [Int]
    counts = map count [0 .. 9]
    count :: Integer -> Int
    count n = length $ filter (== n) xs
    bottom = "==========\n0123456789\n"

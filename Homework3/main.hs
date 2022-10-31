{-# OPTIONS_GHC -Wall #-}

module Golf where

skips :: [a] -> [[a]]
skips [] = []
skips list = map (`skipN` list) [0 .. length list - 1]
  where
    skipN :: Int -> [a] -> [a]
    skipN 0 = id
    skipN n = loop n 0

    loop :: Int -> Int -> [a] -> [a]
    loop _ _ [] = []
    loop x acc (y : ys)
      | x == acc = y : loop x 0 ys
      | otherwise = loop x (acc + 1) ys

localMaxima :: [Integer] -> [Integer]
localMaxima (x : xs@(y : z : _))
  | y > x && y > z = y : localMaxima xs
  | otherwise = localMaxima xs
localMaxima _ = []

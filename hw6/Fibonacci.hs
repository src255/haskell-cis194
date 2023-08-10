{-# OPTIONS_GHC -Wno-missing-methods #-}

module Fibonacci where

import           Data.List (intercalate)

fib :: Integer -> Integer
fib n
    | n <= 0 = 0
    | n == 1 = 1
    | otherwise = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0 ..]

fibs2 :: [Integer]
fibs2 = 0 : 1 : [a + b | (a, b) <- zip fibs2 (tail fibs2)]

data Stream a =
    Stream a (Stream a)

instance Show a => Show (Stream a) where
    show :: Show a => Stream a -> String
    show = intercalate ", " . map show . take 20 . streamToList

streamToList :: Stream a -> [a]
streamToList (Stream x s) = x : streamToList s

streamRepeat :: a -> Stream a
streamRepeat x = s
  where
    s = Stream x s

instance Functor Stream where
    fmap :: (a -> b) -> Stream a -> Stream b
    fmap f (Stream x xs) = Stream (f x) (fmap f xs)

-- streamMap :: (a -> b) -> Stream a -> Stream b
-- streamMap f (Stream a s) = Stream (f a) (streamMap f s)
streamZip :: Stream a -> Stream b -> Stream (a, b)
streamZip (Stream x xs) (Stream y ys) = Stream (x, y) (streamZip xs ys)

streamZipWith :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
streamZipWith f (Stream x xs) (Stream y ys) = Stream (f x y) (streamZipWith f xs ys)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Stream x tail
  where
    tail = Stream (f x) (fmap f tail)

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams = left
  where
    left (Stream x xs) ys = Stream x (right xs ys)
    right xs (Stream y ys) = Stream y (left xs ys)

nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

largestPowOf2 :: Integer -> Integer
largestPowOf2 n
    | n == 0 || odd n = 0
    | otherwise = 1 + largestPowOf2 (n `div` 2)

ruler :: Stream Integer
ruler = largestPowOf2 <$> streamFromSeed (+ 1) 1

ruler' :: Stream Integer
ruler' = weave 0
  where
    weave n = interleaveStreams (streamRepeat n) (weave (n + 1))

x :: Stream Integer
x = Stream 0 (Stream 1 (streamRepeat 0))

instance Num (Stream Integer) where
    fromInteger :: Integer -> Stream Integer
    fromInteger n = Stream n (streamRepeat 0)
    negate :: Stream Integer -> Stream Integer
    negate = fmap negate
    (+) :: Stream Integer -> Stream Integer -> Stream Integer
    (+) = streamZipWith (+)
    (*) :: Stream Integer -> Stream Integer -> Stream Integer
    (Stream x xs) * b@(Stream y ys) = Stream (x * y) (fmap (* x) ys + xs * b)

instance Fractional (Stream Integer) where
    (/) :: Stream Integer -> Stream Integer -> Stream Integer
    a@(Stream x xs) / b@(Stream y ys) = Stream (x `div` y) (fmap (`div` y) (xs - ys * (a / b)))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x ^ 2)

module Fibonacci
    (
    ) where

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

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream a s) = Stream (f a) (streamMap f s)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Stream x tail
  where
    tail = Stream (f x) (streamMap f tail)

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams s s' = left s s'
  where
    left (Stream x s) s' = Stream x (right s s')
    right s' (Stream x s) = Stream x (left s s')

nats :: Stream Integer
nats = streamFromSeed (+ 1) 0
-- ruler :: Stream Integer
-- ruler =

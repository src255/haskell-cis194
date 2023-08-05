module Homework1 where

fibs :: [Integer]
fibs = 0 : 1 : [a + b | (a, b) <- zip fibs (tail fibs)]

--Validating Credit Card Numbers
toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | n < 10 = [n]
  | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | n < 10 = [n]
  | otherwise = n `mod` 10 : toDigitsRev (n `div` 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x1 : x2 : xs)
  | even (length (x1 : x2 : xs)) = (x1 * 2) : x2 : doubleEveryOther xs
  | otherwise = x1 : (x2 * 2) : doubleEveryOther xs

sumDigits :: [Integer] -> Integer
sumDigits list = sum (map digitSum list)
  where
    digitSum :: Integer -> Integer
    digitSum n
      | n < 10 = n
      | otherwise = n `mod` 10 + digitSum (n `div` 10)

validate :: Integer -> Bool
validate n = sumDigits (doubleEveryOther (toDigits n)) `mod` 10 == 0

--Hanoi Towers
type Peg = String

type Move = (Peg, Peg)

hanoi :: Int -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
  | n <= 0 = []
  | n == 1 = [(a, b)]
  | otherwise = hanoi (n - 1) a c b ++ [(a, b)] ++ hanoi (n - 1) c b a

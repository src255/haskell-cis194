{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use map" #-}
{-# HLINT ignore "Eta reduce" #-}
module Homework4
    (
    ) where

import           Data.Set (fromList, toList, (\\))

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
    | n <= 0 = 0
    | even n = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' n = sumEvens $ iterate collatz n
  where
    sumEvens = sum . filter even . takeWhile (> 1)

collatz :: Integer -> Integer
collatz n
    | n <= 0 = 0
    | even n = n `div` 2
    | otherwise = 3 * n + 1

data Tree a
    = Leaf
    | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

insert :: a -> Tree a -> Tree a
insert x Leaf = Node 0 Leaf x Leaf
insert x (Node n Leaf v Leaf) = Node (n + 1) (insert x Leaf) v Leaf
insert x (Node n Leaf v r) = Node n (insert x Leaf) v r
insert x (Node n l v Leaf) = Node n l v (insert x Leaf)
insert x (Node n l@(Node n' _ _ _) v r@(Node n'' _ _ _))
    | n' <= n'' =
        case insert x l of
            Leaf             -> error "insert always creates a Node"
            t@(Node n _ _ _) -> Node (n + 1) t v r
    | otherwise =
        case insert x r of
            Leaf             -> error "insert always creates a Node"
            t@(Node n _ _ _) -> Node (n + 1) l v t

xor :: [Bool] -> Bool
xor = foldl (/=) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base xs

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map adjust filtered
  where
    adjust x = 2 * x + 1
    filtered = toList $ nums \\ exclude
    nums = fromList [1 .. n]
    exclude = fromList [f (i, j) | i <- [1 .. n], j <- [1 .. i], p (i, j)]
    p (i, j) = f (i, j) <= n
    f (i, j) = i + j + 2 * i * j

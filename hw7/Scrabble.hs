{-# LANGUAGE MultiWayIf #-}

module Scrabble where

import           Data.Char (toLower)

newtype Score =
    Score Int
    deriving (Eq, Show)

instance Semigroup Score where
    (<>) :: Score -> Score -> Score
    (Score n) <> (Score n') = Score $ n + n'

instance Monoid Score where
    mempty :: Score
    mempty = Score 0

getScore :: Score -> Int
getScore (Score n) = n

score :: Char -> Score
score c =
    Score
        $ if | c' `elem` "aeilnorstu" -> 1
             | c' == 'd'              -> 2
             | c' `elem` "bcmp"       -> 3
             | c' `elem` "fhvwy"      -> 4
             | c' == 'k'              -> 5
             | c' `elem` "jx"         -> 8
             | c' `elem` "qz"         -> 10
             | otherwise              -> 0
  where
    c' = toLower c

scoreString :: String -> Score
scoreString = foldMap score

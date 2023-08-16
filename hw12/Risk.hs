#!/usr/bin/env stack
-- stack script --resolver lts-21.7 --package MonadRandom
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import           Control.Monad.Random
import           Data.List            (sortOn)

------------------------------------------------------------
-- Die values
newtype DieValue = DV
    { unDV :: Int
    } deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
    random = first DV . randomR (1, 6)
    randomR (low, hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

rolls :: Int -> Rand StdGen [DieValue]
rolls n = sortOn negate <$> replicateM n die

------------------------------------------------------------
-- Risk
type Army = Int

data Battlefield = Battlefield
    { attackers :: Army
    , defenders :: Army
    }

-- | Element-wise operations
instance Num a => Num (a, a) where
    (x, y) + (x', y') = (x + x', y + y')
    (x, y) * (x', y') = (x * x', y * y')
    abs (x, y) = (abs x, abs y)
    negate (x, y) = (negate x, negate y)
    signum (x, y) = (signum x, signum y)
    fromInteger n = (fromInteger n, fromInteger n)

-- | Turn dice rolls into score for attacker and defender
faceOff ::
       [DieValue] -- ^ Attacker rolls
    -> [DieValue] -- ^ Defender rolls
    -> (Army, Army) -- ^ (Attackers lost, Defenders lost)
faceOff xs ys
    | null xs || null ys = (0, 0)
faceOff (x:xs) (y:ys) =
    let start =
            if unDV x > unDV y
                then (0, 1)
                else (1, 0)
     in start + faceOff xs ys

battle :: Battlefield -> Rand StdGen Battlefield
battle b@(Battlefield a d)
    | a >= 2 && d > 0 = do
        as <- rolls $ min (a - 1) 3
        ds <- rolls $ min d 2
        let (a', d') = faceOff as ds
        return Battlefield {attackers = a - a', defenders = d - d'}
    | otherwise = return b

invade :: Battlefield -> Rand StdGen Battlefield
invade b@(Battlefield a d)
    | a < 2 || d == 0 = return b
    | otherwise = do
        b' <- battle b
        invade b'

victory :: Battlefield -> Bool
victory b = defenders b == 0

successProb :: Battlefield -> Rand StdGen Double
successProb b = do
    results <- replicateM 1000 (invade b)
    let victories = length $ filter victory results
    return $ fromIntegral victories / 1000

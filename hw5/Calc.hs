module Calc
    (
    ) where

import           ExprT  (ExprT (..))
import           Parser (parseExp)

class Expr e where
    lit :: Integer -> e
    add :: e -> e -> e
    mul :: e -> e -> e

instance Expr ExprT where
    lit :: Integer -> ExprT
    lit = Lit
    add :: ExprT -> ExprT -> ExprT
    add = Add
    mul :: ExprT -> ExprT -> ExprT
    mul = Mul

instance Expr Integer where
    lit :: Integer -> Integer
    lit = id
    add :: Integer -> Integer -> Integer
    add = (+)
    mul :: Integer -> Integer -> Integer
    mul = (*)

instance Expr Bool where
    lit :: Integer -> Bool
    lit = (> 0)
    add :: Bool -> Bool -> Bool
    add = (||)
    mul :: Bool -> Bool -> Bool
    mul = (&&)

newtype MinMax =
    MinMax Integer
    deriving (Eq, Ord, Show)

instance Expr MinMax where
    lit :: Integer -> MinMax
    lit = MinMax
    add :: MinMax -> MinMax -> MinMax
    add = max
    mul :: MinMax -> MinMax -> MinMax
    mul = min

newtype Mod7 =
    Mod7 Integer
    deriving (Eq, Ord, Show)

instance Expr Mod7 where
    lit :: Integer -> Mod7
    lit n = Mod7 $ n `mod` 7
    add :: Mod7 -> Mod7 -> Mod7
    add (Mod7 n) (Mod7 m) = Mod7 $ (n + m) `mod` 7
    mul :: Mod7 -> Mod7 -> Mod7
    mul (Mod7 n) (Mod7 m) = Mod7 $ (n * m) `mod` 7

eval :: ExprT -> Integer
eval e =
    case e of
        Lit n   -> n
        Add l r -> eval l + eval r
        Mul l r -> eval l * eval r

evalStr :: String -> Maybe Integer
evalStr s = do
    e <- parseExp Lit Add Mul s
    return (eval e)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger :: Maybe Integer
testInteger = testExp :: Maybe Integer

testBool :: Maybe Bool
testBool = testExp :: Maybe Bool

testMM :: Maybe MinMax
testMM = testExp :: Maybe MinMax

testSat :: Maybe Mod7
testSat = testExp :: Maybe Mod7

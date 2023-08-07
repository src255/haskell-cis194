{-# LANGUAGE TypeSynonymInstances #-}

module Calc
    (
    ) where

import qualified Data.Map as M
import           ExprT    (ExprT)
import qualified ExprT    as E
import           Parser   (parseExp)
import qualified StackVM  as SVM
import           StackVM  (Program, stackVM)
import qualified VarExprT as V
import           VarExprT (VarExprT)

class Expr e where
    lit :: Integer -> e
    add :: e -> e -> e
    mul :: e -> e -> e

instance Expr ExprT where
    lit :: Integer -> ExprT
    lit = E.Lit
    add :: ExprT -> ExprT -> ExprT
    add = E.Add
    mul :: ExprT -> ExprT -> ExprT
    mul = E.Mul

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
        E.Lit n   -> n
        E.Add l r -> eval l + eval r
        E.Mul l r -> eval l * eval r

evalStr :: String -> Maybe Integer
evalStr s = do
    e <- parseExp E.Lit E.Add E.Mul s
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

instance Expr Program where
    lit :: Integer -> Program
    lit n = [SVM.PushI n]
    add :: Program -> Program -> Program
    add p p' = p <> p' <> [SVM.Add]
    mul :: Program -> Program -> Program
    mul p p' = p <> p' <> [SVM.Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul

class HasVars a where
    var :: String -> a

instance Expr VarExprT where
    lit :: Integer -> VarExprT
    lit = V.Lit
    add :: VarExprT -> VarExprT -> VarExprT
    add = V.Add
    mul :: VarExprT -> VarExprT -> VarExprT
    mul = V.Mul

instance HasVars VarExprT where
    var :: String -> VarExprT
    var = V.Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var :: String -> M.Map String Integer -> Maybe Integer
    var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
    lit :: Integer -> M.Map String Integer -> Maybe Integer
    lit n _ = Just n
    add :: (M.Map String Integer -> Maybe Integer)
        -> (M.Map String Integer -> Maybe Integer)
        -> M.Map String Integer
        -> Maybe Integer
    add f g m = (+) <$> f m <*> g m
    mul :: (M.Map String Integer -> Maybe Integer)
        -> (M.Map String Integer -> Maybe Integer)
        -> M.Map String Integer
        -> Maybe Integer
    mul f g m = (*) <$> f m <*> g m

withVars :: [(String, Integer)] -> (M.Map String Integer -> Maybe Integer) -> Maybe Integer
withVars vs exp = exp $ M.fromList vs

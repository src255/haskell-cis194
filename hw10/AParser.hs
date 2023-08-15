{-# LANGUAGE LambdaCase #-}

module AParser where

import           Control.Applicative (Alternative (..))
import           Data.Char           (isDigit, isSpace, isUpper)
import           Data.Functor        (void)

newtype Parser a = P
    { parse :: String -> Maybe (a, String)
    }

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f (P p) =
        P $ \input ->
            case p input of
                Nothing     -> Nothing
                Just (x, s) -> Just (f x, s)

instance Applicative Parser where
    pure :: a -> Parser a
    pure x = P $ const $ Just (x, [])
    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    pf <*> pa =
        P $ \input ->
            case parse pf input of
                Nothing     -> Nothing
                Just (f, s) -> parse (f <$> pa) s

instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    (P p) >>= f =
        P $ \input ->
            case p input of
                Nothing     -> Nothing
                Just (x, s) -> parse (f x) s

instance Alternative Parser where
    empty :: Parser a
    empty = P $ const Nothing
    (<|>) :: Parser a -> Parser a -> Parser a
    (P p) <|> (P p') =
        P $ \input ->
            case p input of
                Nothing     -> p' input
                Just (x, s) -> Just (x, s)

item :: Parser Char
item =
    P $ \case
        [] -> Nothing
        x:xs -> Just (x, xs)

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = P f
  where
    f [] = Nothing
    f (x:xs)
        | p x = Just (x, xs)
        | otherwise = Nothing

char :: Char -> Parser Char
char c = satisfy (== c)

posInt :: Parser Integer
posInt = P f
  where
    f xs
        | null ns = Nothing
        | otherwise = Just (read ns, rest)
      where
        (ns, rest) = span isDigit xs

abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = void abParser

intPair :: Parser (Integer, Integer)
intPair = (,) <$> posInt <* satisfy isSpace <*> posInt

intOrUpper :: Parser ()
intOrUpper = void posInt <|> void (satisfy isUpper)

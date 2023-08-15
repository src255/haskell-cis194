module SExpr where

import           AParser             (Parser, char, posInt, satisfy)
import           Control.Applicative ((<|>))
import           Data.Char           (isAlpha, isAlphaNum, isSpace)
import           Data.Function       ((&))
import           Data.Functor        (void)

many :: Parser a -> Parser [a]
many p = some p <|> pure []

some :: Parser a -> Parser [a]
some p = (:) <$> p <*> many p

space :: Parser ()
space = void (satisfy isSpace)

spaces :: Parser ()
spaces = void (many space)

letter :: Parser Char
letter = satisfy isAlpha

alNum :: Parser Char
alNum = satisfy isAlphaNum

ident :: Parser String
ident = (:) <$> letter <*> many alNum

type Ident = String

data Atom
    = N Integer
    | I Ident
    deriving (Show)

data SExpr
    = A Atom
    | Comb [SExpr]
    deriving (Show)

parseAtom :: Parser Atom
parseAtom = N <$> posInt <|> I <$> ident

parseSExpr :: Parser SExpr
parseSExpr =
    A <$> (spaces *> parseAtom <* spaces)
        <|> Comb
                <$> (spaces
                         *> char '('
                         *> spaces
                         *> many parseSExpr
                         <* spaces
                         <* char ')'
                         <* spaces)

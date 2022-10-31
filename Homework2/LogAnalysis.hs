{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log
import Text.ParserCombinators.Parsec as P

messageParser :: GenParser Char st LogMessage
messageParser =
  do
    a <- oneOf "IW"
    _ <- space
    time <- read <$> many1 digit
    _ <- space
    msg <- manyTill anyChar eof
    return
      ( case a of
          'I' -> LogMessage Info time msg
          'W' -> LogMessage Warning time msg
      )
    <|> do
      _ <- char 'E'
      _ <- space
      err <- read <$> many1 digit
      _ <- space
      time <- read <$> many1 digit
      _ <- space
      msg <- manyTill anyChar eof
      return (LogMessage (Error err) time msg)
    <|> do
      text <- manyTill anyChar eof
      return (Unknown text)

parseMessage :: String -> LogMessage
parseMessage s =
  case P.parse messageParser "Error" s of
    Right msg -> msg
    Left _ -> Unknown ""

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert message Leaf = Node Leaf message Leaf
insert message (Node _ (Unknown _) _) = Node Leaf message Leaf
insert
  message@(LogMessage _ time _)
  (Node left root@(LogMessage _ rootTime _) right)
    | time < rootTime = Node (insert message left) root right
    | time > rootTime = Node left root (insert message right)
    | -- overwrite LogMessage if timestamp is equal
      otherwise =
      Node left message right

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left root right) = inOrder left ++ [root] ++ inOrder right

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong messages = map getMessage sortedErrorMessages
  where
    getMessage :: LogMessage -> String
    getMessage (LogMessage _ _ message) = message
    getMessage (Unknown _) = ""

    sortedErrorMessages = inOrder $ build $ filter isSevere messages
      where
        isSevere :: LogMessage -> Bool
        isSevere (LogMessage (Error severity) _ _)
          | severity > 50 = True
        isSevere _notSevere = False

{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

-- getNum :: String -> (Maybe Int, String)
-- getNum s = let details = break(== ' ') s
--            in case details of
--                 (x, ' ':xs) -> (readMaybe x, xs)
--                 _invalidLogMessage           -> (Nothing, "")

-- parseMessage :: String -> LogMessage
-- parseMessage text@('I':' ':rest) =  case time of
--     Nothing -> Unknown text
--     Just x  -> LogMessage Info x message
--     where
--         (time, message) = getNum rest
-- parseMessage text@('W':' ':rest) = case time of
--     Nothing -> Unknown text
--     Just x  -> LogMessage Warning x message
--     where
--         (time, message) = getNum rest
-- parseMessage text@('E':' ':rest) = case severity of
--     Nothing -> Unknown text
--     Just sev -> case time of
--         Nothing -> Unknown text
--         Just t  -> LogMessage (Error sev) t message
--     where
--         (severity, details) = getNum rest
--         (time, message) = getNum details
-- parseMessage text = Unknown text

parseMessage :: String -> LogMessage
parseMessage text@('I' : ' ' : rest) = case words rest of
    (time : message) -> LogMessage Info (read time) (unwords message)
    _invalidLogMessage -> Unknown text
parseMessage text@('W' : ' ' : rest) = case words rest of
    (time : message) -> LogMessage Warning (read time) (unwords message)
    _invalidLogMessage -> Unknown text
parseMessage text@('E' : ' ' : rest) = case words rest of
    (severity : time : message) -> LogMessage (Error (read severity)) (read time) (unwords message)
    _invalidLogMessage -> Unknown text
parseMessage text = Unknown text

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert message Leaf = Node Leaf message Leaf
insert message (Node _ (Unknown _) _) = Node Leaf message Leaf
insert message@(LogMessage _ time _) (Node left root@(LogMessage _ rootTime _) right)
    | time < rootTime = Node (insert message left) root right
    | time > rootTime = Node left root (insert message right)
    -- overwrite LogMessage if timestamp is equal
    | otherwise = Node left message right

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
    getMessage _noMessage = ""

    sortedErrorMessages = inOrder $ build $ filter isSevere messages
      where
        isSevere :: LogMessage -> Bool
        isSevere (LogMessage (Error severity) _ _)
            | severity > 50 = True
        isSevere _notSevere = False

module JoinList where

import           Buffer
import           Scrabble
import           Sized

data JoinList m a
    = Empty
    | Single m a
    | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m

instance Monoid m => Semigroup (JoinList m a) where
    (<>) :: JoinList m a -> JoinList m a -> JoinList m a
    xs <> Empty = xs
    Empty <> xs = xs
    xs <> ys    = Append (tag xs <> tag ys) xs ys

instance Monoid m => Monoid (JoinList m a) where
    mempty :: Monoid m => JoinList m a
    mempty = Empty

instance Foldable (JoinList m) where
    foldMap :: Monoid b => (a -> b) -> JoinList m a -> b
    foldMap f xs =
        case xs of
            Empty          -> mempty
            Single _ a     -> f a
            Append _ xs ys -> foldMap f xs <> foldMap f ys

instance (Sized m, Monoid m) => Sized (JoinList m a) where
    size :: JoinList m a -> Size
    size = size . tag

(+++) :: Monoid a1 => JoinList a1 a2 -> JoinList a1 a3 -> a1
l +++ r = tag l <> tag r

toSize :: Sized a => a -> Int
toSize = getSize . size

(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i
    | i < 0 = Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i - 1)

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ 0 (Single b a) = Just a
indexJ n (Append b l r)
    | n < toSize l = indexJ n l
    | n < toSize b = indexJ (n - toSize l) r
indexJ _ _ = Nothing

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n xs
    | n <= 0 = xs
dropJ n (Append b l r)
    | n < toSize l = Append (tag l' <> tag r) l' r
    | remaining >= 0 = dropJ remaining r
  where
    l' = dropJ n l
    remaining = n - toSize l
dropJ _ _ = Empty

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n xs
    | n >= toSize xs = xs
takeJ n (Append b l r)
    | n <= toSize l = takeJ n l
    | n < toSize b = Append (tag l <> tag r') l r'
  where
    r' = takeJ (n - toSize l) r
takeJ _ _ = Empty

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

instance Buffer (JoinList (Score, Size) String) where
    toString :: JoinList (Score, Size) String -> String
    toString Empty          = []
    toString (Single _ s)   = s
    toString (Append _ l r) = toString l <> toString r
    fromString :: String -> JoinList (Score, Size) String
    fromString = foldMap (\s -> Single (scoreString s, Size 1) s) . lines
    line :: Int -> JoinList (Score, Size) String -> Maybe String
    line = indexJ
    replaceLine :: Int -> String -> JoinList (Score, Size) String -> JoinList (Score, Size) String
    replaceLine i s xs = takeJ (i - 1) xs <> fromString s <> dropJ i xs
    numLines :: JoinList (Score, Size) String -> Int
    numLines Empty          = 0
    numLines (Single _ _)   = 1
    numLines (Append _ l r) = getSize $ size l <> size r
    value :: JoinList (Score, Size) String -> Int
    value = getScore . fst . tag

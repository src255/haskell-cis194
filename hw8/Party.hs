module Party where

import           Data.List                (maximumBy, sort)
import           Data.Ord                 (comparing)
import           Data.Tree                (Tree (..))
import           Distribution.Simple.Test (test)
import           Employee                 (Employee (empFun, empName), Fun,
                                           GuestList (..), testCompany,
                                           testCompany2)
import           Text.Printf              (printf)

glCons :: Employee -> GuestList -> GuestList
glCons e g@(GL es fun)
    | e `notElem` es = GL (e : es) (empFun e + fun)
    | otherwise = g

instance Semigroup GuestList where
    (<>) :: GuestList -> GuestList -> GuestList
    (GL es _) <> g = foldr glCons g es

instance Monoid GuestList where
    mempty :: GuestList
    mempty = GL mempty 0

fun :: GuestList -> Fun
fun (GL _ f) = f

moreFun :: GuestList -> GuestList -> GuestList
moreFun g g'
    | fun g >= fun g' = g
    | otherwise = g'

treeFold :: Monoid b => (a -> [b] -> b) -> Tree a -> b
treeFold f (Node a as) = f a (map (treeFold f) as)

flattenWith :: (a -> c) -> (b -> c) -> [(a, b)] -> [c]
flattenWith f g xs = map (f . fst) xs ++ map (g . snd) xs

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel b [] = (GL [b] (empFun b), mempty)
nextLevel b xs = (withBoss, withoutBoss)
  where
    withBoss = maximumBy (comparing fun) (flattenWith awkward ok xs)
    withoutBoss = maximumBy (comparing fun) (flattenWith id id xs)
    awkward (GL (e:es) f) = GL (b : e : es) (f - empFun e)
    ok = glCons b

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel

guests :: GuestList -> String
guests (GL es f) = printf "Fun: %d\n" f <> unlines (sort $ map empName es)

main :: IO ()
main = do
    company <- readFile "company.txt"
    -- let company = testCompany
    putStrLn $ guests $ maxFun $ read company

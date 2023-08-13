#!/usr/bin/env stack
-- stack script --resolver lts --package hspec --package QuickCheck
import           Control.Monad
import           Data.Foldable
import           JoinList
import           Sized
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

joinList :: Gen (JoinList Size Int)
joinList = sized joinList'

joinList' 0 = return Empty
joinList' n
    | n > 0 =
        oneof
            [ Single (Size 1) <$> arbitrary
            , do
                  l <- subList
                  r <- subList
                  return $ Append (size l <> size r) l r
            ]
  where
    subList = joinList' (n `div` 2)

gen = do
    n <- arbitrary
    xs <- joinList
    return (n, xs)

main :: IO ()
main =
    hspec $ do
        describe "JoinList" $ do
            modifyMaxSuccess (const 10000) $ do
                prop "indexJ should behave like (!!?)"
                    $ forAll gen
                    $ \(i, xs) -> indexJ i xs `shouldBe` toList xs !!? i
                prop "dropJ should behave like drop"
                    $ forAll gen
                    $ \(n, xs) -> toList (dropJ n xs) `shouldBe` drop n (toList xs)
                prop "takeJ should behave like take"
                    $ forAll gen
                    $ \(n, xs) -> toList (takeJ n xs) `shouldBe` take n (toList xs)

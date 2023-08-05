#!/usr/bin/env stack
-- stack script --resolver lts --package hspec
import           Homework4
import           Test.Hspec
import           Test.Hspec.QuickCheck

main :: IO ()
main =
    hspec $ do
        describe "Function equality" $ do
            modifyMaxSuccess (const 1000) $ do
                prop "fun1' should be fun1" $ \xs -> fun1' xs `shouldBe` fun1 xs
                prop "fun2' should be fun2" $ \x -> fun2' x `shouldBe` fun2 x
        describe "correctness" $
            modifyMaxSuccess (const 1000) $
            prop "xor works" $ do \xs -> xor xs `shouldBe` (length (filter id xs) `mod` 2 == 1)

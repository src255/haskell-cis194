#!/usr/bin/env stack
-- stack script --resolver lts --package hspec
import           Homework4
import           Test.Hspec
import           Test.Hspec.QuickCheck

main :: IO ()
main =
    hspec $ do
        describe "Function equality" $ do
            modifyMaxSuccess (const 1000) $
                prop "fun1' should be fun1" $ \xs -> fun1' xs `shouldBe` fun1 xs
            -- modifyMaxSuccess (const 1000) $
            --     prop "fun2' should be fun2" $ \xs -> fun2' xs `shouldBe` fun2 xs

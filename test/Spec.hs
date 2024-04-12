{-
-- EPITECH PROJECT, 2024
-- MyTh-ophileJ-r-me.Doc
-- File description:
-- Spec
-}

import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "add" $ do

        it "should add two numbers" $ do
            (1 :: Int) + 1 `shouldBe` 2
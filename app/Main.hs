{-
-- EPITECH PROJECT, 2023
-- MyPandoc
-- File description:
-- Main
-}

import System.Environment (getArgs)

import MyPandoc

main :: IO ()
main = do
    args <- getArgs
    myPandoc args
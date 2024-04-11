{-
-- EPITECH PROJECT, 2023
-- MyPandoc
-- File description:
-- MyPandoc
-}

module MyPandoc (
    myPandoc
    ) where

import System.Exit(exitWith, ExitCode(ExitFailure))

import ParsingCommand

myPandoc :: [String] -> IO ()
myPandoc [] = exitWith(ExitFailure 84)
myPandoc args = case parseArgs args of
                          Nothing -> displayUsage >> exitWith(ExitFailure 84)
                          Just conf -> print conf
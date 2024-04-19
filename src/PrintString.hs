{-
-- EPITECH PROJECT, 2024
-- MyTh-ophileJ-r-me.Doc
-- File description:
-- PrintString
-}

module PrintString (
    printString,
    myFromJustString
    ) where

import System.IO

myFromJustString :: Maybe String -> String
myFromJustString (Just str) = str
myFromJustString Nothing = ""

printString :: Maybe Handle -> String -> Int -> IO ()
printString Nothing str spaces = putStr (replicate spaces ' ') >> putStr str
printString (Just handle) str spaces = hPutStr handle (replicate spaces ' ') >>
    hPutStr handle str

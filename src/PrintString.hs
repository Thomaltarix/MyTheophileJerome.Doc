{-
-- EPITECH PROJECT, 2024
-- MyTh-ophileJ-r-me.Doc
-- File description:
-- PrintString
-}

module PrintString (
    printString,
    myFromJustString,
    -- printEnd,
    getString,
    getEnd
    ) where

import System.IO

getEnd :: Bool -> String
getEnd end
    | not end = ",\n"
    | otherwise = "\n"

myFromJustString :: Maybe String -> String
myFromJustString (Just str) = str
myFromJustString Nothing = ""

getString :: String -> Int -> String
getString str spaces = replicate spaces ' ' ++ str

printString :: Maybe Handle -> String -> IO ()
printString Nothing str = putStr str
printString (Just handle) str = hPutStr handle str

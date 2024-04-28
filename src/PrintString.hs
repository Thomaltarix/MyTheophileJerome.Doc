{-
-- EPITECH PROJECT, 2024
-- MyTh-ophileJ-r-me.Doc
-- File description:
-- PrintString
-}

module PrintString (
    printString,
    myFromJustString,
    getString,
    getEnd,
    printEnd
    ) where

import System.IO

printEnd :: Maybe Handle -> Bool -> IO ()
printEnd Nothing _ = return ()
printEnd (Just handle) end = hPutStr handle (getEnd end)

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

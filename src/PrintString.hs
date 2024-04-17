{-
-- EPITECH PROJECT, 2024
-- MyTh-ophileJ-r-me.Doc
-- File description:
-- PrintString
-}

module PrintString (
    printString
    ) where

import System.IO

printString :: Maybe Handle -> String -> IO ()
printString Nothing str = putStrLn str
printString (Just handle) str = hPutStrLn handle str

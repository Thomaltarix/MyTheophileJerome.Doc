{-
-- EPITECH PROJECT, 2023
-- MyPandoc
-- File description:
-- ParseJson
-}

module Json.ParseJson (
    jsonParsing
    ) where

import Json.ParseHeader(getHeader)
import Json.ParseContent
import DataStructure

-- test :: IO ()
-- test = readFile "example/syntaxe.md" >>= print . jsonParsing

jsonParsing :: String -> Maybe DataStruct
jsonParsing str =
    case getHeader str defaultHeader of
        (Just h, str') -> case getContent str' of
            (Just c, _) -> Just DataStruct {
                header = h,
                content = c
                }
            (Nothing, _) -> Nothing
        (Nothing, _) -> Nothing

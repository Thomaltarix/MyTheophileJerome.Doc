{-
-- EPITECH PROJECT, 2023
-- MyPandoc
-- File description:
-- ParseJson
-}

module Xml.ParseXml (
    xmlParsing,
    test
    ) where

import Xml.ParseHeader(getHeader)
import DataStructure

test :: IO ()
test = readFile "example/syntaxe.xml" >>= print . xmlParsing

xmlParsing :: String -> Maybe DataStruct
xmlParsing str =
    case getHeader str defaultHeader of
        (Just h, _) -> Just DataStruct {
                header = h,
                content = defaultObject
                }
        (Nothing, _) -> Nothing

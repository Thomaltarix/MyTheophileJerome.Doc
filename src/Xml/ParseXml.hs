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
import Xml.ParseContent(getContent)
import DataStructure ( defaultHeader, DataStruct(..) )

test :: IO ()
test = readFile "example/syntaxe.xml" >>= print . xmlParsing

xmlParsing :: String -> Maybe DataStruct
xmlParsing str =
    case getHeader str defaultHeader of
        (Just h, str') -> case getContent str' of
            (Just c, _) -> Just DataStruct {
                header = h,
                content = c
                }
            (Nothing, _) -> Nothing
        (Nothing, _) -> Nothing
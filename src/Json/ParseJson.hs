{-
-- EPITECH PROJECT, 2023
-- MyPandoc
-- File description:
-- ParseJson
-}

module Json.ParseJson (
    ) where

import Json.ParseHeader
import DataStructure
-- import DataStructure

data JsonValue = JsonVoid | JsonTitle | JsonAuthor | JsonDate deriving (Show)

test :: IO ()
test = readFile "example/syntaxe.json" >>= print . jsonParsing

jsonParsing :: String -> DataStruct
jsonParsing str =
    case getHeader str defaultHeader of
        Just h -> DataStruct {
                header = h,
                content = createObject SectionT "test" [] [] }
        Nothing -> DataStruct {
                header = defaultHeader,
                content = createObject SectionT "test" [] []}

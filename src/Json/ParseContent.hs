{-
-- EPITECH PROJECT, 2023
-- MyPandoc
-- File description:
-- ParseContent
-}

module Json.ParseContent (
    getContent
    ) where

-- import ParsingLib
import DataStructure
import ParsingLib

getContent :: String -> Object -> (Maybe Object, String)
getContent str o = (Just o, str)

jsonTextParsing :: String -> Parser String
jsonTextParsing str = do
    _ <- parseString str
    _ <- parseMany (parseAnyChar " \n\t")
    a <- parseStringQuote
    _ <- parseMany (parseAnyChar " \n\t,")
    return a

-- jsonArrayParsing :: String -> Parser String
-- jsonArrayParsing str = 
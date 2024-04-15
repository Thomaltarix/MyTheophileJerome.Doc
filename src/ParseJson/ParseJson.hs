{-
-- EPITECH PROJECT, 2023
-- MyPandoc
-- File description:
-- ParseJson
-}

module ParseJson.ParseJson (
    ) where

import ParsingLib
-- import DataStructure

data JsonValue = JsonNull
               | JsonBool Bool
               | JsonNumber Double
               | JsonString String
               deriving (Show)

test :: IO ()
test = readFile "example/syntaxe.json" >>= print . (runParser jsonHeaderParsing)

jsonTextParsing :: String -> Parser String
jsonTextParsing str = do
    _ <- parseString str
    _ <- parseMany (parseAnyChar " \n\t")
    a <- parseStringQuote
    _ <- parseMany (parseAnyChar " \n\t,")
    return a

jsonHeaderParsing :: Parser (String, String, String)
jsonHeaderParsing = do
    _ <- parseAnd (parseChar '{') (parseMany (parseAnyChar " \n\t"))
    _ <- parseString "\"header\": {"
    _ <- parseMany (parseAnyChar " \n\t")
    a <- jsonTextParsing "\"title\":" <|> jsonTextParsing "\"author\":" <|> jsonTextParsing "\"date\":"
    b <- jsonTextParsing "\"title\":" <|> jsonTextParsing "\"author\":" <|> jsonTextParsing "\"date\":"
    c <- jsonTextParsing "\"title\":" <|> jsonTextParsing "\"author\":" <|> jsonTextParsing "\"date\":"
    return (a, b, c)

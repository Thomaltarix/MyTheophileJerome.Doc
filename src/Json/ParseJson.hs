{-
-- EPITECH PROJECT, 2023
-- MyPandoc
-- File description:
-- ParseJson
-}

module Json.ParseJson (
    ) where

import ParsingLib
-- import DataStructure

data JsonValue = JsonVoid | JsonTitle | JsonAuthor | JsonDate deriving (Show)

test :: IO ()
test = readFile "example/syntaxe.json" >>= print . (runParser jsonHeaderParsing)

jsonTextParsing :: String -> Parser String
jsonTextParsing str = do
    _ <- parseString str
    _ <- parseMany (parseAnyChar " \n\t")
    a <- parseStringQuote
    _ <- parseMany (parseAnyChar " \n\t,")
    return a

jsonTitleParsing :: Parser (JsonValue, String)
jsonTitleParsing = do
    _ <- parseString "\"title\":"
    _ <- parseMany (parseAnyChar " \n\t")
    a <- parseStringQuote
    _ <- parseMany (parseAnyChar " \n\t,")
    return (JsonTitle, a)

jsonAuthorParsing :: Parser (JsonValue, String)
jsonAuthorParsing = do
    _ <- parseString "\"author\":"
    _ <- parseMany (parseAnyChar " \n\t")
    a <- parseStringQuote
    _ <- parseMany (parseAnyChar " \n\t,")
    return (JsonAuthor, a)

jsonDateParsing :: Parser (JsonValue, String)
jsonDateParsing = do
    _ <- parseString "\"date\":"
    _ <- parseMany (parseAnyChar " \n\t")
    a <- parseStringQuote
    _ <- parseMany (parseAnyChar " \n\t,")
    return (JsonDate, a)

jsonVoidParsing :: Parser (JsonValue, String)
jsonVoidParsing = return (JsonVoid, "")

jsonHeaderParsing :: Parser ((JsonValue, String), (JsonValue, String), (JsonValue, String))
jsonHeaderParsing = do
    _ <- parseAnd (parseChar '{') (parseMany (parseAnyChar " \n\t"))
    _ <- parseString "\"header\": {"
    _ <- parseMany (parseAnyChar " \n\t")
    a <- jsonTitleParsing <|> jsonAuthorParsing <|> jsonDateParsing <|> jsonVoidParsing
    b <- jsonTitleParsing <|> jsonAuthorParsing <|> jsonDateParsing <|> jsonVoidParsing
    c <- jsonTitleParsing <|> jsonAuthorParsing <|> jsonDateParsing <|> jsonVoidParsing
    return (a, b, c)

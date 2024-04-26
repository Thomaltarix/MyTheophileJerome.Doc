{-
-- EPITECH PROJECT, 2023
-- MyPandoc
-- File description:
-- ParseJson
-}

module Json.ParseHeader (
    getHeader,
    ) where

import ParsingLib
import DataStructure

data JsonValue = JsonVoid | JsonTitle | JsonAuthor | JsonDate deriving (Show)

getHeader :: String -> Header -> (Maybe Header, String)
getHeader str h =
    case jsonHeader str h of
        (Header {title = Nothing}, _) -> (Nothing, str)
        (h', str') -> (Just h', str')

jsonHeader :: String -> Header -> (Header, String)
jsonHeader str h =
    case runParser jsonHeaderParsing str of
        Just (((aType, aData), (bType, bData), (cType, cData)), str') ->
            let h1 = processJson (aType, aData) h
                h2 = processJson (bType, bData) h1
                h3 = processJson (cType, cData) h2
            in (h3, str')
        Nothing -> (h, str)

jsonHeaderParsing :: Parser ((JsonValue, String), (JsonValue, String), (JsonValue, String))
jsonHeaderParsing = do
    _ <- parseAnd (parseChar '{') (parseMany (parseAnyChar " \n\t"))
    _ <- parseString "\"header\":"
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseAnd (parseChar '{') (parseMany (parseAnyChar " \n\t"))
    a <- jsonTitleParse<|>jsonAuthorParse<|>jsonDateParse<|>jsonVoidParse
    b <- jsonTitleParse<|>jsonAuthorParse<|>jsonDateParse<|>jsonVoidParse
    c <- jsonTitleParse<|>jsonAuthorParse<|>jsonDateParse<|>jsonVoidParse
    _ <- parseAnd (parseChar '}') (parseMany (parseAnyChar " \n\t,"))
    return (a, b, c)

jsonTitleParse :: Parser (JsonValue, String)
jsonTitleParse = do
    _ <- parseString "\"title\":"
    _ <- parseMany (parseAnyChar " \n\t")
    a <- parseStringQuote
    _ <- parseMany (parseAnyChar " \n\t,")
    return (JsonTitle, a)

jsonAuthorParse :: Parser (JsonValue, String)
jsonAuthorParse = do
    _ <- parseString "\"author\":"
    _ <- parseMany (parseAnyChar " \n\t")
    a <- parseStringQuote
    _ <- parseMany (parseAnyChar " \n\t,")
    return (JsonAuthor, a)

jsonDateParse :: Parser (JsonValue, String)
jsonDateParse = do
    _ <- parseString "\"date\":"
    _ <- parseMany (parseAnyChar " \n\t")
    a <- parseStringQuote
    _ <- parseMany (parseAnyChar " \n\t,")
    return (JsonDate, a)

jsonVoidParse :: Parser (JsonValue, String)
jsonVoidParse = return (JsonVoid, "")

processJsonTitle :: String -> Header -> Header
processJsonTitle str h =
    h {title = Just (createData (Just str) TextT (Just "title"))}

processJsonAuthor :: String -> Header -> Header
processJsonAuthor str h =
    h {author = Just (createData (Just str) TextT (Just "author"))}

processJsonDate :: String -> Header -> Header
processJsonDate str h =
    h {date = Just (createData (Just str) TextT (Just "date"))}

processJson :: (JsonValue, String) -> Header -> Header
processJson (JsonTitle, jsonData)  h = processJsonTitle jsonData
    (addHeaderOrder "title" h)
processJson (JsonAuthor, jsonData) h = processJsonAuthor jsonData
    (addHeaderOrder "author" h)
processJson (JsonDate, jsonData) h = processJsonDate jsonData
    (addHeaderOrder "date" h)
processJson _ h = h

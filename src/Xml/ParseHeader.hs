{-
-- EPITECH PROJECT, 2023
-- MyPandoc
-- File description:
-- Parsexml
-}

module Xml.ParseHeader (
    getHeader
    ) where

import ParsingLib
import DataStructure

data XmlValue = XmlVoid | XmlTitle | XmlAuthor | XmlDate deriving (Show)

getHeader :: String -> Header -> (Maybe Header, String)
getHeader str h =
    case xmlHeader str h of
        (Header {title = Nothing}, _) -> (Nothing, str)
        (h', str') -> (Just h', str')

xmlHeader :: String -> Header -> (Header, String)
xmlHeader str h =
    case runParser xmlHeaderParsing str of
        Just (((aType, aData), (bType, bData), (cType, cData)), str') ->
            let h1 = processxml (aType, aData) h
                h2 = processxml (bType, bData) h1
                h3 = processxml (cType, cData) h2
            in (h3, str')
        Nothing -> (h, str)

xmlHeaderParsing :: Parser ((XmlValue, String), (XmlValue, String), (XmlValue, String))
xmlHeaderParsing = do
    _ <- parseString "<document>"
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseString "<header"
    _ <- parseMany (parseChar ' ')
    a <- xmlTitleParse<|>xmlAuthorParse<|>xmlDateParse<|>xmlVoidParse
    b <- xmlTitleParse<|>xmlAuthorParse<|>xmlDateParse<|>xmlVoidParse
    c <- xmlTitleParse<|>xmlAuthorParse<|>xmlDateParse<|>xmlVoidParse
    _ <- parseString "</header>"
    return (a, b, c)

xmlTitleParse :: Parser (XmlValue, String)
xmlTitleParse = do
    _ <- parseString "title="
    a <- parseStringQuote
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseChar '>'
    _ <- parseMany (parseAnyChar " \n\t")
    return (XmlTitle, a)

xmlAuthorParse :: Parser (XmlValue, String)
xmlAuthorParse = do
    _ <- parseMany (parseAnyChar " \n\t")
    a <- parseStringTag "author"
    _ <- parseMany (parseAnyChar " \n\t")
    return (XmlAuthor, a)

xmlDateParse :: Parser (XmlValue, String)
xmlDateParse = do
    _ <- parseMany (parseAnyChar " \n\t")
    a <- parseStringTag "date"
    _ <- parseMany (parseAnyChar " \n\t")
    return (XmlDate, a)

xmlVoidParse :: Parser (XmlValue, String)
xmlVoidParse = return (XmlVoid, "")

processXmlTitle :: String -> Header -> Header
processXmlTitle str h =
    h {title = Just (createData (Just str) TextT (Just "title"))}

processXmlAuthor :: String -> Header -> Header
processXmlAuthor str h =
    h {author = Just (createData (Just str) TextT (Just "author"))}

processXmlDate :: String -> Header -> Header
processXmlDate str h =
    h {date = Just (createData (Just str) TextT (Just "date"))}

processxml :: (XmlValue, String) -> Header -> Header
processxml (XmlTitle, xmlData)  h = processXmlTitle xmlData h
processxml (XmlAuthor, xmlData) h = processXmlAuthor xmlData h
processxml (XmlDate, xmlData) h = processXmlDate xmlData h
processxml _ h = h

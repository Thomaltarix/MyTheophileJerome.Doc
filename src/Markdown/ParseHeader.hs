{-
-- EPITECH PROJECT, 2023
-- MyPandoc
-- File description:
-- ParseMarkdown
-}

module Markdown.ParseHeader (
    getHeader,
    ) where

import ParsingLib
import DataStructure

data MarkdownValue = MarkdownVoid | MarkdownTitle | MarkdownAuthor | MarkdownDate deriving (Show)

getHeader :: String -> Header -> (Maybe Header, String)
getHeader str h =
    case markdownHeader str h of
        (Header {title = Nothing}, _) -> (Nothing, str)
        (h', str') -> (Just h', str')

markdownHeader :: String -> Header -> (Header, String)
markdownHeader str h =
    case runParser markdownHeaderParsing str of
        Just (((aType, aData), (bType, bData), (cType, cData)), str') ->
            let h1 = processMarkdown (aType, aData) h
                h2 = processMarkdown (bType, bData) h1
                h3 = processMarkdown (cType, cData) h2
            in (h3, str')
        Nothing -> (h, str)

markdownHeaderParsing :: Parser ((MarkdownValue, String), (MarkdownValue, String), (MarkdownValue, String))
markdownHeaderParsing = do
    _ <- parseString "---"
    _ <- parseMany (parseAnyChar "\n \t")
    a <- markdownTitleParse <|> markdownAuthorParse <|> markdownDateParse <|> markdownVoidParse
    b <- markdownTitleParse <|> markdownAuthorParse <|> markdownDateParse <|> markdownVoidParse
    c <- markdownTitleParse <|> markdownAuthorParse <|> markdownDateParse <|> markdownVoidParse
    _ <- parseString "---\n"
    return (a, b, c)

markdownTitleParse :: Parser (MarkdownValue, String)
markdownTitleParse = do
    _ <- parseString "title: "
    a <- parseUntilChar '\n'
    _ <- parseMany (parseAnyChar "\n \t")
    return (MarkdownTitle, a)

markdownAuthorParse :: Parser (MarkdownValue, String)
markdownAuthorParse = do
    _ <- parseString "author: "
    a <- parseUntilChar '\n'
    _ <- parseMany (parseAnyChar "\n \t")
    return (MarkdownAuthor, a)

markdownDateParse :: Parser (MarkdownValue, String)
markdownDateParse = do
    _ <- parseString "date: "
    a <- parseUntilChar '\n'
    _ <- parseMany (parseAnyChar "\n \t")
    return (MarkdownDate, a)

markdownVoidParse :: Parser (MarkdownValue, String)
markdownVoidParse = return (MarkdownVoid, "")

processMarkdownTitle :: String -> Header -> Header
processMarkdownTitle str h =
    h {title = Just (createData (Just str) TextT (Just "title"))}

processMarkdownAuthor :: String -> Header -> Header
processMarkdownAuthor str h =
    h {author = Just (createData (Just str) TextT (Just "author"))}

processMarkdownDate :: String -> Header -> Header
processMarkdownDate str h =
    h {date = Just (createData (Just str) TextT (Just "date"))}

processMarkdown :: (MarkdownValue, String) -> Header -> Header
processMarkdown (MarkdownTitle, markdownData)  h = processMarkdownTitle markdownData h
processMarkdown (MarkdownAuthor, markdownData) h = processMarkdownAuthor markdownData h
processMarkdown (MarkdownDate, markdownData) h = processMarkdownDate markdownData h
processMarkdown _ h = h

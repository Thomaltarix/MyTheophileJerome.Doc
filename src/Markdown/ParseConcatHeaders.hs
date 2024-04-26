{-
-- EPITECH PROJECT, 2023
-- MyPandoc
-- File description:
-- ParseConcatHeaders
-}
module Markdown.ParseConcatHeaders where

import ParsingLib

import DataStructure

import Markdown.ParseParagraph

import Markdown.ParseBlocks

concatList :: Int -> Parser [Either Data Object]
concatList level =
    concat <$> parseMany ((:[]) <$> (parseHeaderBd level 6 "###### "
    <|> parseHeaderBd level 5 "##### " <|> parseHeaderBd level 4 "#### "
    <|> parseHeaderBd level 3 "### " <|> parseHeaderBd level 2 "## "
    <|> parseHeaderBd level 1 "# " <|> parseCodeBlock 
    <|> parseListBlock  <|> parseParagraph ))

wrapInEmptySection :: [Either Data Object] -> Object
wrapInEmptySection o = (createObject SectionT Nothing [
            Right (createObject SectionT (Just "section") [
                (Left (createData (Just "") TextT (Just "title"))),
                (Right (createObject ListT (Just "content") o))
            ])])

createHeaderSection :: String -> [Either Data Object] -> Object
createHeaderSection title o = (createObject SectionT Nothing [
            Right (createObject SectionT (Just "section") [
                (Left (createData (Just title) TextT (Just "title"))),
                (Right (createObject ListT (Just "content") o))
            ])
        ])

parseHeaderBd :: Int -> Int -> String -> Parser (Either Data Object)
parseHeaderBd clevel thislevel trigger 
    | ((thislevel - (clevel + 1)) > 0) = do
        a <- parseHeaderBd (clevel + 1) thislevel trigger
        return (Right (wrapInEmptySection [a]))
    | clevel < thislevel = do
    _ <- parseMany (parseAnyChar "\n \t")
    _ <- parseString trigger
    title <- parseUntilChar '\n'
    content <- concatList thislevel
    return (Right (createHeaderSection title content))
    | otherwise = do
        _ <- failingParse
        return (Left (createData (Just "Error") TextT Nothing))
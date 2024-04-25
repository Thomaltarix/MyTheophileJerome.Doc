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
    concat <$> parseMany ((:[]) <$> (parseHeaderSix level
    <|> parseHeaderFive level <|> parseHeaderFour level 
    <|> parseHeaderThree level <|> parseHeaderTwo level 
    <|> parseHeaderOne level <|> parseCodeBlock 
    <|> parseListBlock  <|> parseParagraph ))

parseHeaderOne :: Int -> Parser (Either Data Object)
parseHeaderOne num 
    | ((1 - (num + 1)) > 0) = do
        a <- parseHeaderOne (num + 1)
        return (Right (createObject SectionT Nothing [
            Right (createObject SectionT (Just "section") [
                (Left (createData (Just "") TextT (Just "title"))),
                (Right (createObject ListT (Just "content") [a]))
            ])]))
    | num < 1 = do
    _ <- parseMany (parseAnyChar "\n \t")
    _ <- parseString "# "
    title <- parseUntilChar '\n'
    content <- concatList 1
    return (Right (createObject SectionT Nothing [
            Right (createObject SectionT (Just "section") [
                (Left (createData (Just title) TextT (Just "title"))),
                (Right (createObject ListT (Just "content") content))
            ])
        ]))
    | otherwise = do
        _ <- failingParse
        return (Left (createData (Just "Error") TextT Nothing))

parseHeaderTwo :: Int -> Parser (Either Data Object)
parseHeaderTwo num 
    | ((2 - (num + 1)) > 0) = do
        a <- parseHeaderTwo (num + 1)
        return (Right (createObject SectionT Nothing [
            Right (createObject SectionT (Just "section") [
                (Left (createData (Just "") TextT (Just "title"))),
                (Right (createObject ListT (Just "content") [a]))
            ])]))
    | num < 2 = do
    _ <- parseMany (parseAnyChar "\n \t")
    _ <- parseString "## "
    title <- parseUntilChar '\n'
    content <- concatList 2
    return (Right (createObject SectionT Nothing [
            Right (createObject SectionT (Just "section") [
                (Left (createData (Just title) TextT (Just "title"))),
                (Right (createObject ListT (Just "content") content))
            ])
        ]))
    | otherwise = do
        _ <- failingParse
        return (Left (createData (Just "Error") TextT Nothing))

parseHeaderThree :: Int -> Parser (Either Data Object)
parseHeaderThree num 
    | ((3 - (num + 1)) > 0) = do
        a <- parseHeaderThree (num + 1)
        return (Right (createObject SectionT Nothing [
            Right (createObject SectionT (Just "section") [
                (Left (createData (Just "") TextT (Just "title"))),
                (Right (createObject ListT (Just "content") [a]))
            ])]))
    | num < 3 = do
    _ <- parseMany (parseAnyChar "\n \t")
    _ <- parseString "### "
    title <- parseUntilChar '\n'
    content <- concatList 3
    return (Right (createObject SectionT Nothing [
            Right (createObject SectionT (Just "section") [
                (Left (createData (Just title) TextT (Just "title"))),
                (Right (createObject ListT (Just "content") content))
            ])
        ]))
    | otherwise = do
        _ <- failingParse
        return (Left (createData (Just "Error") TextT Nothing))

parseHeaderFour :: Int -> Parser (Either Data Object)
parseHeaderFour num 
    | ((4 - (num + 1)) > 0) = do
        a <- parseHeaderFour (num + 1)
        return (Right (createObject SectionT Nothing [
            Right (createObject SectionT (Just "section") [
                (Left (createData (Just "") TextT (Just "title"))),
                (Right (createObject ListT (Just "content") [a]))
            ])]))
    | num < 4 = do
    _ <- parseMany (parseAnyChar "\n \t")
    _ <- parseString "#### "
    title <- parseUntilChar '\n'
    content <- concatList 4
    return (Right (createObject SectionT Nothing [
            Right (createObject SectionT (Just "section") [
                (Left (createData (Just title) TextT (Just "title"))),
                (Right (createObject ListT (Just "content") content))
            ])
        ]))
    | otherwise = do
        _ <- failingParse
        return (Left (createData (Just "Error") TextT Nothing))

parseHeaderFive :: Int -> Parser (Either Data Object)
parseHeaderFive num 
    | ((5 - (num + 1)) > 0) = do
        a <- parseHeaderFive (num + 1)
        return (Right (createObject SectionT Nothing [
            Right (createObject SectionT (Just "section") [
                (Left (createData (Just "") TextT (Just "title"))),
                (Right (createObject ListT (Just "content") [a]))
            ])]))
    | num < 5 = do
    _ <- parseMany (parseAnyChar "\n \t")
    _ <- parseString "##### "
    title <- parseUntilChar '\n'
    content <- concatList 5
    return (Right (createObject SectionT Nothing [
            Right (createObject SectionT (Just "section") [
                (Left (createData (Just title) TextT (Just "title"))),
                (Right (createObject ListT (Just "content") content))
            ])
        ]))
    | otherwise = do
        _ <- failingParse
        return (Left (createData (Just "Error") TextT Nothing))

parseHeaderSix :: Int -> Parser (Either Data Object)
parseHeaderSix num 
    | ((6 - (num + 1)) > 0) = do
        a <- parseHeaderSix (num + 1)
        return (Right (createObject SectionT Nothing [
            Right (createObject SectionT (Just "section") [
                (Left (createData (Just "") TextT (Just "title"))),
                (Right (createObject ListT (Just "content") [a]))
            ])]))
    | num < 6 = do
    _ <- parseMany (parseAnyChar "\n \t")
    _ <- parseString "###### "
    title <- parseUntilChar '\n'
    content <- concatList 6
    return (Right (createObject SectionT Nothing [
            Right (createObject SectionT (Just "section") [
                (Left (createData (Just title) TextT (Just "title"))),
                (Right (createObject ListT (Just "content") content))
            ])
        ]))
    | otherwise = do
        _ <- failingParse
        return (Left (createData (Just "Error") TextT Nothing))

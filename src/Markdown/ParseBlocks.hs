{-
-- EPITECH PROJECT, 2023
-- MyPandoc
-- File description:
-- ParseBlocks
-}

module Markdown.ParseBlocks (
    parseListBlock,
    parseListItem,
    parseCodeBlock,
    parseCodeblockItem
) where

import DataStructure

import ParsingLib

import Markdown.ParseParagraph

parseListBlock :: Parser (Either Data Object)
parseListBlock = do
    _ <- parseMany (parseAnyChar "\n \t")
    _ <- checkIfChar '-'
    o <- concat <$> parseMany ((:[]) <$> (parseListItem))
    return (Right (createObject SectionT Nothing 
        [Right (createObject ListT (Just "list") o)]))

parseListItem :: Parser (Either Data Object)
parseListItem = do
    _ <- parseString "- "
    _ <- parseMany (parseAnyChar " \t")
    c <- parseParagraph
    _ <- parseMany (parseAnyChar " \t")
    return (c)

parseCodeBlock :: Parser (Either Data Object)
parseCodeBlock = do
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseString "```"
    _ <- parseMany (parseAnyChar " \n\t")
    a <- concat <$> parseMany ((:[]) <$> (parseCodeblockItem))
    _ <- parseString "```"
    _ <- parseMany (parseAnyChar " \n\t")
    return (Right (createObject SectionT Nothing [Right (createObject
        CodeBlockT (Just "codeblock") 
        [(Right (createObject ListT Nothing a))])]))

parseCodeblockItem :: Parser (Either Data Object)
parseCodeblockItem = do
    _ <- parseMany (parseAnyChar " \n\t")
    c <- parseSome (parseAnyCharNotMatch "`\n")
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseMany (parseAnyChar " \n\t")
    return (Left (createData (Just c) TextT Nothing))

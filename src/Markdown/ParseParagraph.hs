{-
-- EPITECH PROJECT, 2023
-- MyPandoc
-- File description:
-- ParseParagraph
-}

module Markdown.ParseParagraph (
    parseParagraph,
) where

import ParsingLib

import DataStructure

parseParagraph :: Parser (Either Data Object)
parseParagraph = do
    _ <- parseMany (parseAnyChar "\n \t")
    o <- concat <$> parseMany ((:[]) <$> (parseBold <|> parseItalic
        <|> parseImage <|> parseLink <|> parseCode
        <|> parseText   ))
    _ <- parseString "\n"
    return (Right (createObject ListT Nothing o))

parseText :: Parser (Either Data Object)
parseText = do
    t <- parseSome (parseAnyCharNotMatch "\n`*[!-#")
    return (Left (createData (Just t) TextT Nothing))

parseItalic :: Parser (Either Data Object)
parseItalic = do
    _ <- parseChar '*'
    s <- parseUntilChar '*'
    _ <- checkNotChar '\0'
    return (Right (createObject SectionT Nothing [Left (createData (Just s)
        ItalicT (Just "italic"))]))

parseBold :: Parser (Either Data Object)
parseBold = do
    _ <- parseString "**"
    b <- parseUntilTwoChar "**"
    return (Right (createObject SectionT Nothing [Left (createData (Just b)
        BoldT (Just "bold"))]))

parseCode :: Parser (Either Data Object)
parseCode = do
    _ <- parseChar '`'
    c <- parseUntilChar '`'
    return (Right (createObject SectionT Nothing [Left (createData (Just c)
        CodeT (Just "code"))]))

parseImage :: Parser (Either Data Object)
parseImage = do
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseString "!["
    alt <- parseUntilChar ']'
    _ <- parseChar '('
    url <- parseUntilChar ')'
    return $ Right $ createObject SectionT Nothing [Right (createObject
        ImageT (Just "image") [Left (createData (Just url) TextT (Just "url"))
        , Right (createObject ListT (Just "alt") [Left (createData (Just alt)
        TextT Nothing)])])]

parseLink :: Parser (Either Data Object)
parseLink = do
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseChar '['
    gcontent <- parseUntilChar ']'
    _ <- parseChar '('
    url <- parseUntilChar ')'
    return $ Right $ createObject SectionT Nothing [Right (createObject
        LinkT (Just "link") [Left (createData (Just url) TextT (Just "url")),
        Right (createObject ListT (Just "content") [Left (createData 
        (Just gcontent) TextT Nothing)])])]


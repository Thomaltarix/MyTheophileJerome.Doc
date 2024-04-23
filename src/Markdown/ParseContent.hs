{-
-- EPITECH PROJECT, 2023
-- MyPandoc
-- File description:
-- ParseContent
-}

module Markdown.ParseContent where

import ParsingLib

import DataStructure


getContent :: String -> (Maybe Object, String)
getContent str = case runParser parseMarkdownBody str of
    Nothing -> (Nothing, str)
    Just (c, str') -> (Just c, str')

parseMarkdownBody :: Parser Object
parseMarkdownBody = do
    _ <- parseMany (parseAnyChar " \n\t")
    a <- concatList
    _ <- parseMany (parseAnyChar " \n\t")
    return (createObject ListT (Just "body") a)

concatList :: Parser [Either Data Object]
concatList =
    concat <$> parseMany ((:[]) <$> ( parseSection <|>
        parseCodeBlock <|> parseList <|> parseParagraph))

parseParagraph :: Parser (Either Data Object)
parseParagraph = do
    o <- concat <$> parseMany ((:[]) <$> (parseText <|> parseItalic <|>
        parseBold <|> parseCode <|> parseLink <|> parseImage))
    _ <- parseMany (parseAnyChar " \n\t")
    return (Right (createObject ListT Nothing o))

parseText :: Parser (Either Data Object)
parseText = do
    t <- parseUntilChar '\n'
    return (Left (createData (Just t) TextT Nothing))




parseItalic :: Parser (Either Data Object)
parseItalic = do
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseString "*"
    s <- parseUntilChar '*'
    _ <- parseMany (parseAnyChar " \n\t")
    return (Right (createObject SectionT Nothing [Left (createData (Just s)
        BoldT (Just "italic"))]))

parseBold :: Parser (Either Data Object)
parseBold = do
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseString "**"
    b <- parseUntilString "**"
    _ <- parseMany (parseAnyChar " \n\t")
    return (Right (createObject SectionT Nothing [Left (createData (Just b)
        BoldT (Just "bold"))]))

parseCode :: Parser (Either Data Object)
parseCode = do
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseString "`"
    c <- parseUntilChar '`'
    _ <- parseMany (parseAnyChar " \n\t")
    return (Right (createObject SectionT Nothing [Left (createData (Just c)
        BoldT (Just "code"))]))

-- parseTitle :: Parser (Either Data Object)
-- parseTitle = do
--     _ <- parseMany (parseAnyChar " \n\t")
--     _ <- parseString "title="
--     t <- parseStringQuote
--     _ <- parseMany (parseAnyChar " \n\t>")
--     return (Left (createData (Just t) TextT (Just "title")))

-- parseSection :: Parser (Either Data Object)
-- parseSection = do
--     _ <- parseMany (parseAnyChar " \n\t")
--     _ <- parseString "<section"
--     title <- parseTitle
--     content <- concatList
--     _ <- parseString "</section>"
--     _ <- parseMany (parseAnyChar " \n\t")
--     return (Right (createObject SectionT Nothing [Right (createObject SectionT
--         (Just "section") [title, Right (createObject ListT (Just "content")
--         content)])]))

parseCodeBlock :: Parser (Either Data Object)
parseCodeBlock = do
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseString "```"
    content <- parseUntilString "```"
    _ <- parseMany (parseAnyChar " \n\t")
    return (Right (createObject SectionT Nothing [Right (createObject
        CodeBlockT (Just "codeblock") content)]))

-- parseList :: Parser (Either Data Object)
-- parseList = do
--     _ <- parseMany (parseAnyChar " \n\t")
--     _ <- parseString "<list>"
--     _ <- parseMany (parseAnyChar " \n\t")
--     content <- concatList
--     _ <- parseMany (parseAnyChar " \n\t")
--     _ <- parseString "</list>"
--     _ <- parseMany (parseAnyChar " \n\t")
--     return (Right (createObject SectionT Nothing [Right (createObject
--         ListT (Just "list") content)]))

-- concatLink :: Parser Object
-- concatLink = do
--     a <- concat <$> parseMany ((:[]) <$> (parseUrl <|> parseContentLink))
--     return defaultObject{objType = LinkT, objSymbol = Just "link", datas = a}

-- parseUrl :: Parser (Either Data Object)
-- parseUrl = do
--     _ <- parseMany (parseAnyChar " \n\t")
--     _ <- parseString "url="
--     u <- parseStringQuote
--     _ <- parseMany (parseAnyChar " \n\t")
--     _ <- parseChar '>'
--     _ <- parseMany (parseAnyChar " \n\t")
--     return (Left (createData (Just u) TextT (Just "url")))

-- parseContentLink :: Parser (Either Data Object)
-- parseContentLink = do
--     _ <- parseMany (parseAnyChar " \n\t")
--     c <- parseSome (parseAnyCharNotMatch "<")
--     return (Right (createObject ListT (Just "content") [Left (createData
--         (Just c) TextT Nothing)]))

-- parseLink :: Parser (Either Data Object)
-- parseLink = do
--     _ <- parseMany (parseAnyChar " \n\t")
--     _ <- parseString "<link"
--     _ <- parseMany (parseAnyChar " \n\t")
--     content <- concatLink
--     _ <- parseMany (parseAnyChar " \n\t")
--     _ <- parseString "</link>"
--     _ <- parseMany (parseAnyChar " \n\t")
--     return (Right (createObject SectionT Nothing [Right content]))

-- parseAlt :: Parser (Either Data Object)
-- parseAlt = do
--     _ <- parseMany (parseAnyChar " \n\t")
--     c <- parseSome (parseAnyCharNotMatch "<")
--     return (Right (createObject AltT (Just "alt") [Left (createData (Just c)
--         TextT Nothing)]))

-- concatImage :: Parser Object
-- concatImage = do
--     a <- concat <$> parseMany ((:[]) <$> (parseUrl <|> parseAlt))
--     return defaultObject{objType = ImageT, objSymbol = Just "image", datas = a}

-- parseImage :: Parser (Either Data Object)
-- parseImage = do
--     _ <- parseMany (parseAnyChar " \n\t")
--     _ <- parseString "<image"
--     _ <- parseMany (parseAnyChar " \n\t")
--     content <- concatImage
--     _ <- parseMany (parseAnyChar " \n\t")
--     _ <- parseString "</image>"
--     _ <- parseMany (parseAnyChar " \n\t")
--     return (Right (createObject SectionT Nothing [Right content]))
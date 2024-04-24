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
getContent str = case runParser parseMdBody str of
    Nothing -> (Nothing, str)
    Just (c, str') -> (Just c, str')

parseMdBody :: Parser Object
parseMdBody = do
    a <- concatList
    return (createObject ListT (Just "body") a)

concatList :: Parser [Either Data Object]
concatList =
    concat <$> parseMany ((:[]) <$> (parseCodeBlock <|> parseParagraph )) -- <|>  <|>parseSection   <|> parseList)


parseParagraph :: Parser (Either Data Object)
parseParagraph = do
    o <- concat <$> parseMany ((:[]) <$> (parseBold <|> parseItalic <|> parseCode <|> parseText   )) --  <|> parseLink <|> parseImage 
    _ <- parseString "\n"
    return (Right (createObject ListT Nothing o))

parseText :: Parser (Either Data Object)
parseText = do
    t <- parseSome (parseAnyCharNotMatch "\n`*") -- #
    return (Left (createData (Just t) TextT Nothing))

parseItalic :: Parser (Either Data Object)
parseItalic = do
    _ <- parseChar '*'
    s <- parseUntilChar '*'
    return (Right (createObject SectionT Nothing [Left (createData (Just s)
        BoldT (Just "italic"))]))

parseBold :: Parser (Either Data Object)
parseBold = do
    _ <- parseString "**"
    b <- parseUntilString "**"
    return (Right (createObject SectionT Nothing [Left (createData (Just b)
        BoldT (Just "bold"))]))

parseCode :: Parser (Either Data Object)
parseCode = do
    _ <- parseChar '`'
    _ <- checkNotChar '`'
    c <- parseUntilChar '`'
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
--     gcontent <- concatList
--     _ <- parseString "</section>"
--     _ <- parseMany (parseAnyChar " \n\t")
--     return (Right (createObject SectionT Nothing [Right (createObject SectionT
--         (Just "section") [title, Right (createObject ListT (Just "gcontent")
--         gcontent)])]))

parseCodeBlock :: Parser (Either Data Object)
parseCodeBlock = do
    -- _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseString "```"
    c <- parseUntilString "```"
    _ <- parseChar '`' -- MUST TO SKIP FIN WTF 
    -- _ <- parseMany (parseAnyChar " \n\t")
    return (Right (createObject SectionT Nothing [Left (createData (Just c)
        CodeT (Just "code"))]))

-- parseList :: Parser (Either Data Object)
-- parseList = do
--     _ <- parseMany (parseAnyChar " \n\t")
--     _ <- parseString "<list>"
--     _ <- parseMany (parseAnyChar " \n\t")
--     gcontent <- concatList
--     _ <- parseMany (parseAnyChar " \n\t")
--     _ <- parseString "</list>"
--     _ <- parseMany (parseAnyChar " \n\t")
--     return (Right (createObject SectionT Nothing [Right (createObject
--         ListT (Just "list") gcontent)]))

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
--     return (Right (createObject ListT (Just "gcontent") [Left (createData
--         (Just c) TextT Nothing)]))

-- parseLink :: Parser (Either Data Object)
-- parseLink = do
--     _ <- parseMany (parseAnyChar " \n\t")
--     _ <- parseString "<link"
--     _ <- parseMany (parseAnyChar " \n\t")
--     gcontent <- concatLink
--     _ <- parseMany (parseAnyChar " \n\t")
--     _ <- parseString "</link>"
--     _ <- parseMany (parseAnyChar " \n\t")
--     return (Right (createObject SectionT Nothing [Right gcontent]))

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
--     gcontent <- concatImage
--     _ <- parseMany (parseAnyChar " \n\t")
--     _ <- parseString "</image>"
--     _ <- parseMany (parseAnyChar " \n\t")
--     return (Right (createObject SectionT Nothing [Right gcontent]))
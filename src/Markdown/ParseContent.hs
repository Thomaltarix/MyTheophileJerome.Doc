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


-- {
--                         "section": {
--                             "title": "header 2",
--                             "content": [
parseHeaderOne :: Parser (Either Data Object)
parseHeaderOne = do
    _ <- parseMany (parseAnyChar "\n \t")
    _ <- parseString "# "
    title <- parseUntilChar '\n'
    content <- concatList
    return (Right (createObject SectionT Nothing [
            Right (createObject SectionT (Just "section") [
                (Left (createData (Just title) TextT (Just "title"))),
                (Right (createObject ListT (Just "content") content))
            ])
        ]))

parseHeaderTwo :: Parser (Either Data Object)
parseHeaderTwo = do
    _ <- parseMany (parseAnyChar "\n \t")
    _ <- parseString "## "
    title <- parseUntilChar '\n'
    content <- concatList
    return (Right (createObject SectionT Nothing [
            Right (createObject SectionT (Just "section") [
                (Left (createData (Just title) TextT (Just "title"))),
                (Right (createObject ListT (Just "content") content))
            ])
        ]))

parseHeaderThree :: Parser (Either Data Object)
parseHeaderThree = do
    _ <- parseMany (parseAnyChar "\n \t")
    _ <- parseString "### "
    title <- parseUntilChar '\n'
    content <- concatList
    return (Right (createObject SectionT Nothing [
            Right (createObject SectionT (Just "section") [
                (Left (createData (Just title) TextT (Just "title"))),
                (Right (createObject ListT (Just "content") content))
            ])
        ]))

parseHeaderFour :: Parser (Either Data Object)
parseHeaderFour = do
    _ <- parseMany (parseAnyChar "\n \t")
    _ <- parseString "#### "
    title <- parseUntilChar '\n'
    content <- concatList
    return (Right (createObject SectionT Nothing [
            Right (createObject SectionT (Just "section") [
                (Left (createData (Just title) TextT (Just "title"))),
                (Right (createObject ListT (Just "content") content))
            ])
        ]))

parseHeaderFive :: Parser (Either Data Object)
parseHeaderFive = do
    _ <- parseMany (parseAnyChar "\n \t")
    _ <- parseString "##### "
    title <- parseUntilChar '\n'
    content <- concatList
    return (Right (createObject SectionT Nothing [
            Right (createObject SectionT (Just "section") [
                (Left (createData (Just title) TextT (Just "title"))),
                (Right (createObject ListT (Just "content") content))
            ])
        ]))

parseHeaderSix :: Parser (Either Data Object)
parseHeaderSix = do
    _ <- parseMany (parseAnyChar "\n \t")
    _ <- parseString "###### "
    title <- parseUntilChar '\n'
    content <- concatList
    return (Right (createObject SectionT Nothing [
            Right (createObject SectionT (Just "section") [
                (Left (createData (Just title) TextT (Just "title"))),
                (Right (createObject ListT (Just "content") content))
            ])
        ]))

concatList :: Parser [Either Data Object]
concatList =
    concat <$> parseMany ((:[]) <$> (parseHeaderSix <|> parseHeaderFive <|> parseHeaderFour <|> parseHeaderThree <|> parseHeaderTwo <|> parseHeaderOne <|> parseCodeBlock <|> parseListBlock  <|> parseParagraph ))

parseListBlock :: Parser (Either Data Object)
parseListBlock = do
    _ <- parseMany (parseAnyChar "\n \t")
    _ <- checkIfChar '-'
    o <- concat <$> parseMany ((:[]) <$> (parseListItem))
    return (Right (createObject SectionT Nothing [Right (createObject ListT (Just "list") o)]))

parseListItem :: Parser (Either Data Object)
parseListItem = do
    _ <- parseString "- "
    _ <- parseMany (parseAnyChar " \t")
    c <- parseParagraph
    _ <- parseMany (parseAnyChar " \t")
    return (c)

parseParagraph :: Parser (Either Data Object)
parseParagraph = do
    _ <- parseMany (parseAnyChar "\n \t")
    o <- concat <$> parseMany ((:[]) <$> (parseBold <|> parseItalic <|> parseImage <|> parseLink <|> parseCode <|> parseText   ))
    _ <- parseString "\n"
    return (Right (createObject ListT Nothing o))

parseText :: Parser (Either Data Object)
parseText = do
    t <- parseSome (parseAnyCharNotMatch "\n`*[!-#") -----MAJOR ISSUE TO FIX
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
    c <- parseUntilChar '`'
    return (Right (createObject SectionT Nothing [Left (createData (Just c)
        BoldT (Just "code"))]))

parseCodeBlock :: Parser (Either Data Object) -- 
parseCodeBlock = do
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseString "```"
    _ <- parseMany (parseAnyChar " \t")
    c <- parseUntilString "``"
    _ <- parseChar '`'
    _ <- parseMany (parseAnyChar " \t")
    return (Right (createObject SectionT Nothing [Left (createData (Just c)
        CodeT (Just "codeblock"))]))

parseImage :: Parser (Either Data Object)
parseImage = do
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseChar '!'
    _ <- parseChar '['
    alt <- parseUntilChar ']'
    _ <- parseChar '('
    url <- parseUntilChar ')'
    return $ Right $ createObject SectionT Nothing [Right (createObject ImageT (Just "image") [Left (createData (Just url) TextT (Just "url")), Right (createObject ListT (Just "alt") [Left (createData (Just alt) TextT Nothing)])])]

parseLink :: Parser (Either Data Object)
parseLink = do
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseChar '['
    gcontent <- parseUntilChar ']'
    _ <- parseChar '('
    url <- parseUntilChar ')'
    return $ Right $ createObject SectionT Nothing [Right (createObject LinkT (Just "link") [Left (createData (Just url) TextT (Just "url")), Right (createObject ListT (Just "content") [Left (createData (Just gcontent) TextT Nothing)])])]

-- parseList :: Parser (Either Data Object)
-- parseList = do
--     _ <- parseMany (parseAnyChar " \n\t")
--     _ <- parseString "-"
--     _ <- parseMany (parseAnyChar " \t")
--     gcontent <- parseUntilChar '\n'
--     _ <- parseMany (parseAnyChar " \n\t")
--     return (Right (createObject SectionT Nothing [Right (createObject
--         ListT (Just "list") [])]))

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

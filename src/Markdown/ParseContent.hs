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
    a <- concatList 0
    return (createObject ListT (Just "body") a)

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

concatList :: Int -> Parser [Either Data Object]
concatList level =
    concat <$> parseMany ((:[]) <$> (parseHeaderSix level <|> parseHeaderFive level <|> parseHeaderFour level <|> parseHeaderThree level <|> parseHeaderTwo level <|> parseHeaderOne level <|> parseCodeBlock <|> parseListBlock  <|> parseParagraph ))

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


parseCodeBlock :: Parser (Either Data Object) -- 
parseCodeBlock = do
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseString "```"
    _ <- parseMany (parseAnyChar " \n\t")
    a <- concat <$> parseMany ((:[]) <$> (parseCodeblockItem))
    _ <- parseString "```"
    _ <- parseMany (parseAnyChar " \n\t")
    return (Right (createObject SectionT Nothing [Right (createObject CodeBlockT (Just "codeblock") a)]))

parseCodeblockItem :: Parser (Either Data Object)
parseCodeblockItem = do
    _ <- parseMany (parseAnyChar " \n\t")
    c <- parseSome (parseAnyCharNotMatch "`\n")
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseMany (parseAnyChar " \n\t")
    return (Left (createData (Just c) TextT Nothing))

-- parseListBlock :: Parser (Either Data Object)
-- parseListBlock = do
--     _ <- parseMany (parseAnyChar "\n \t")
--     _ <- checkIfChar '-'
--     o <- concat <$> parseMany ((:[]) <$> (parseListItem))
--     return (Right (createObject SectionT Nothing [Right (createObject ListT (Just "list") o)]))

-- parseListItem :: Parser (Either Data Object)
-- parseListItem = do
--     _ <- parseString "- "
--     _ <- parseMany (parseAnyChar " \t")
--     c <- parseParagraph
--     _ <- parseMany (parseAnyChar " \t")
--     return (c)
{-
-- EPITECH PROJECT, 2023
-- MyPandoc
-- File description:
-- ParseContent
-}

module Markdown.ParseContent (
    getContent
    ) where

-- import ParsingLib
import ParsingLib
import DataStructure

getContent :: String -> (Maybe Object, String)
getContent str = case runParser parseMarkdownBody str of
    Nothing -> (Nothing, str)
    Just (c, str') -> (Just c, str')

concatList :: Maybe String -> ObjectType -> Object -> Parser Object
concatList str t o = do
    a <- concat <$> parseMany ((:[]) <$> (parseMarkdownArray <|> parseMarkdownObject
        <|> parseMarkdownStringQuote <|> parseMarkdownInt))
    return o{objType = t, objSymbol = str, datas = a}

concatObject :: Maybe String -> ObjectType -> Object -> Parser Object
concatObject str t o = do
    a <- concat <$> parseMany ((:[]) <$> (parseMarkdownSection <|> parseMarkdownTitle
        <|> parseMarkdownContent <|> parseMarkdownBold <|> parseMarkdownItalic <|>
        parseMarkdownCode <|> parseMarkdownCodeBlock <|> parseMarkdownList <|> parseMarkdownUrl
        <|> parseMarkdownImage <|> parseMarkdownAlt <|> parseMarkdownLink))
    return o{objType = t, objSymbol = str, datas = a}

parseMarkdownBody :: Parser Object
parseMarkdownBody = do
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseString "\"body\":"
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseAnd (parseChar '[') (parseMany (parseAnyChar " \n\t"))
    a <- concatList (Just "body") ListT defaultObject
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseAnd (parseChar ']') (parseMany (parseAnyChar " \n\t"))
    _ <- parseChar '}'
    return a

parseMarkdownArray :: Parser (Either Data Object)
parseMarkdownArray = do
    _ <- parseAnd (parseChar '[') (parseMany (parseAnyChar " \n\t"))
    a <- concatList Nothing ListT defaultObject
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseChar ']'
    _ <- parseMany (parseAnyChar " \n\t,")
    return (Right a)

parseMarkdownObject :: Parser (Either Data Object)
parseMarkdownObject = do
    _ <- parseAnd (parseChar '{') (parseMany (parseAnyChar " \n\t"))
    a <- concatObject Nothing SectionT defaultObject
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseChar '}'
    _ <- parseMany (parseAnyChar " \n\t,")
    return (Right a)

parseMarkdownSection :: Parser (Either Data Object)
parseMarkdownSection = do
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseString "\"section\":"
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseChar '{'
    _ <- parseMany (parseAnyChar " \n\t")
    a <- concatObject (Just "section") SectionT defaultObject
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseChar '}'
    return (Right a)

parseMarkdownTitle :: Parser (Either Data Object)
parseMarkdownTitle = do
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseString "\"title\":"
    _ <- parseMany (parseAnyChar " \n\t")
    a <- parseStringQuote
    _ <- parseMany (parseAnyChar " \n\t,")
    return (Left (createData (Just a) TextT (Just "title")))

parseMarkdownContent :: Parser (Either Data Object)
parseMarkdownContent = do
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseString "\"content\":"
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseChar '['
    _ <- parseMany (parseAnyChar " \n\t")
    a <- concatList (Just "content") ListT defaultObject
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseChar ']'
    return (Right a)

parseMarkdownBold :: Parser (Either Data Object)
parseMarkdownBold = do
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseString "\"bold\":"
    _ <- parseMany (parseAnyChar " \n\t")
    a <- parseStringQuote
    _ <- parseMany (parseAnyChar " \n\t,")
    return (Left (createData (Just a) BoldT (Just "bold")))

parseMarkdownItalic :: Parser (Either Data Object)
parseMarkdownItalic = do
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseString "\"italic\":"
    _ <- parseMany (parseAnyChar " \n\t")
    a <- parseStringQuote
    _ <- parseMany (parseAnyChar " \n\t,")
    return (Left (createData (Just a) ItalicT (Just "italic")))

parseMarkdownCode :: Parser (Either Data Object)
parseMarkdownCode = do
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseString "\"code\":"
    _ <- parseMany (parseAnyChar " \n\t")
    a <- parseStringQuote
    _ <- parseMany (parseAnyChar " \n\t,")
    return (Left (createData (Just a) CodeT (Just "code")))

parseMarkdownCodeBlock ::Parser (Either Data Object)
parseMarkdownCodeBlock = do
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseString "\"codeblock\":"
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseChar '['
    _ <- parseMany (parseAnyChar " \n\t")
    a <- concatList (Just "codeblock") CodeBlockT defaultObject
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseChar ']'
    return (Right a)

parseMarkdownList :: Parser (Either Data Object)
parseMarkdownList = do
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseString "\"list\":"
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseChar '['
    _ <- parseMany (parseAnyChar " \n\t")
    a <- concatList (Just "list") ListT defaultObject
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseAnd (parseChar ']') (parseMany (parseAnyChar " \n\t,"))
    return (Right a)

parseMarkdownUrl :: Parser (Either Data Object)
parseMarkdownUrl = do
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseString "\"url\":"
    _ <- parseMany (parseAnyChar " \n\t")
    a <- parseStringQuote
    _ <- parseMany (parseAnyChar " \n\t,")
    return (Left (createData (Just a) TextT (Just "url")))

parseMarkdownImage :: Parser (Either Data Object)
parseMarkdownImage = do
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseString "\"image\":"
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseChar '{'
    _ <- parseMany (parseAnyChar " \n\t")
    a <- concatObject (Just "image") ImageT defaultObject
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseChar '}'
    return (Right a)

parseMarkdownAlt :: Parser (Either Data Object)
parseMarkdownAlt = do
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseString "\"alt\":"
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseChar '['
    _ <- parseMany (parseAnyChar " \n\t")
    a <- concatList (Just "alt") AltT defaultObject
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseChar ']'
    return (Right a)

parseMarkdownLink :: Parser (Either Data Object)
parseMarkdownLink = do
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseString "\"link\":"
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseChar '{'
    _ <- parseMany (parseAnyChar " \n\t")
    a <- concatObject (Just "link") LinkT defaultObject
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseChar '}'
    return (Right a)

parseMarkdownStringQuote :: Parser (Either Data Object)
parseMarkdownStringQuote = do
    _ <- parseMany (parseAnyChar " \n\t")
    a <- parseStringQuote
    _ <- parseMany (parseAnyChar " \n\t,")
    return (Left (createData (Just a) TextT Nothing))

parseMarkdownInt :: Parser (Either Data Object)
parseMarkdownInt = do
    _ <- parseMany (parseAnyChar " \n\t")
    a <- parseIntString
    _ <- parseMany (parseAnyChar " \n\t,")
    return (Left (createData (Just a) TextT Nothing))

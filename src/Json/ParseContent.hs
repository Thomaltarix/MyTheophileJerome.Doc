{-
-- EPITECH PROJECT, 2023
-- MyPandoc
-- File description:
-- ParseContent
-}

module Json.ParseContent (
    getContent
    ) where

-- import ParsingLib
import ParsingLib
import DataStructure

data JsonValue = JsonText | JsonTitle | JsonBold |
                JsonItalic | JsonCode | JsonUrl |
                JsonAlt | JsonImage deriving (Show)

getContent :: String -> Object -> (Maybe Object, String)
getContent str o = case runParser parseJsonBody str of
    Nothing -> (Nothing, str)
    Just (c, str') -> (Just c, str')

concatList :: Maybe String -> ObjectType -> Object -> Parser Object
concatList str t o = do
    a <- concat <$> parseMany ((:[]) <$> (parseJsonArray <|> parseJsonObject
        <|> parseJsonStringQuote <|> parseJsonInt))
    return o{objType = t, objSymbol = str, datas = a}

concatObject :: Maybe String -> ObjectType -> Object -> Parser Object
concatObject str t o = do
    a <- concat <$> parseMany ((:[]) <$> (parseJsonSection <|> parseJsonTitle
        <|> parseJsonContent <|> parseJsonBold <|> parseJsonItalic <|>
        parseJsonCode <|> parseJsonCodeBlock <|> parseJsonList <|> parseJsonUrl
        <|> parseJsonImage <|> parseJsonAlt <|> parseJsonLink))
    return o{objType = t, objSymbol = str, datas = a}

parseJsonBody :: Parser Object
parseJsonBody = do
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseString "\"body\":"
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseAnd (parseChar '[') (parseMany (parseAnyChar " \n\t"))
    a <- concatList (Just "body") ListT defaultObject
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseAnd (parseChar ']') (parseMany (parseAnyChar " \n\t"))
    _ <- parseChar '}'
    return a

parseJsonArray :: Parser (Either Data Object)
parseJsonArray = do
    _ <- parseAnd (parseChar '[') (parseMany (parseAnyChar " \n\t"))
    a <- concatList Nothing ListT defaultObject
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseChar ']'
    _ <- parseMany (parseAnyChar " \n\t,")
    return (Right a)

parseJsonObject :: Parser (Either Data Object)
parseJsonObject = do
    _ <- parseAnd (parseChar '{') (parseMany (parseAnyChar " \n\t"))
    a <- concatObject Nothing SectionT defaultObject
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseChar '}'
    _ <- parseMany (parseAnyChar " \n\t,")
    return (Right a)

parseJsonSection :: Parser (Either Data Object)
parseJsonSection = do
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseString "\"section\":"
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseChar '{'
    _ <- parseMany (parseAnyChar " \n\t")
    a <- concatObject (Just "section") SectionT defaultObject
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseChar '}'
    return (Right a)

parseJsonTitle :: Parser (Either Data Object)
parseJsonTitle = do
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseString "\"title\":"
    _ <- parseMany (parseAnyChar " \n\t")
    a <- parseStringQuote
    _ <- parseMany (parseAnyChar " \n\t,")
    return (Left (createData (Just a) TextT (Just "title")))

parseJsonContent :: Parser (Either Data Object)
parseJsonContent = do
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseString "\"content\":"
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseChar '['
    _ <- parseMany (parseAnyChar " \n\t")
    a <- concatList (Just "content") ListT defaultObject
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseChar ']'
    return (Right a)

parseJsonBold :: Parser (Either Data Object)
parseJsonBold = do
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseString "\"bold\":"
    _ <- parseMany (parseAnyChar " \n\t")
    a <- parseStringQuote
    _ <- parseMany (parseAnyChar " \n\t,")
    return (Left (createData (Just a) BoldT (Just "bold")))

parseJsonItalic :: Parser (Either Data Object)
parseJsonItalic = do
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseString "\"italic\":"
    _ <- parseMany (parseAnyChar " \n\t")
    a <- parseStringQuote
    _ <- parseMany (parseAnyChar " \n\t,")
    return (Left (createData (Just a) ItalicT (Just "italic")))

parseJsonCode :: Parser (Either Data Object)
parseJsonCode = do
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseString "\"code\":"
    _ <- parseMany (parseAnyChar " \n\t")
    a <- parseStringQuote
    _ <- parseMany (parseAnyChar " \n\t,")
    return (Left (createData (Just a) CodeT (Just "code")))

parseJsonCodeBlock ::Parser (Either Data Object)
parseJsonCodeBlock = do
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseString "\"codeblock\":"
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseChar '['
    _ <- parseMany (parseAnyChar " \n\t")
    a <- concatList (Just "codeblock") CodeBlockT defaultObject
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseChar ']'
    return (Right a)

parseJsonList :: Parser (Either Data Object)
parseJsonList = do
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseString "\"list\":"
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseChar '['
    _ <- parseMany (parseAnyChar " \n\t")
    a <- concatList Nothing ListT defaultObject
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseAnd (parseChar ']') (parseMany (parseAnyChar " \n\t,"))
    return (Right a)

parseJsonUrl :: Parser (Either Data Object)
parseJsonUrl = do
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseString "\"url\":"
    _ <- parseMany (parseAnyChar " \n\t")
    a <- parseStringQuote
    _ <- parseMany (parseAnyChar " \n\t,")
    return (Left (createData (Just a) TextT (Just "url")))

parseJsonImage :: Parser (Either Data Object)
parseJsonImage = do
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseString "\"image\":"
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseChar '{'
    _ <- parseMany (parseAnyChar " \n\t")
    a <- concatObject (Just "image") SectionT defaultObject
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseChar '}'
    return (Right a)

parseJsonAlt :: Parser (Either Data Object)
parseJsonAlt = do
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseString "\"alt\":"
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseChar '['
    _ <- parseMany (parseAnyChar " \n\t")
    a <- concatList (Just "alt") ListT defaultObject
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseChar ']'
    return (Right a)

parseJsonLink :: Parser (Either Data Object)
parseJsonLink = do
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseString "\"link\":"
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseChar '{'
    _ <- parseMany (parseAnyChar " \n\t")
    a <- concatObject (Just "link") SectionT defaultObject
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseChar '}'
    return (Right a)

parseJsonStringQuote :: Parser (Either Data Object)
parseJsonStringQuote = do
    _ <- parseMany (parseAnyChar " \n\t")
    a <- parseStringQuote
    _ <- parseMany (parseAnyChar " \n\t,")
    return (Left (createData (Just a) TextT Nothing))

parseJsonInt :: Parser (Either Data Object)
parseJsonInt = do
    _ <- parseMany (parseAnyChar " \n\t")
    a <- parseIntString
    _ <- parseMany (parseAnyChar " \n\t,")
    return (Left (createData (Just a) TextT Nothing))

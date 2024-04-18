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
import DataStructure
import ParsingLib

getContent :: String -> Object -> (Maybe Object, String)
getContent str o = case runParser parseJsonBody str of
    Nothing -> (Nothing, str)
    Just _ -> (Just o, str)

parseJsonBody :: Parser String
parseJsonBody = do
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseString "\"body\":"
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseChar '['
    _ <- parseMany (parseAnyChar " \n\t")
    a <- concat <$> parseMany (parseJsonStringQuote <|> parseJsonInt <|>
        parseJsonArray <|> parseJsonObject <|> parseJsonLink <|>
        parseJsonImage <|> parseJsonSection <|>
        parseJsonCodeBlock <|> parseJsonList)
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseChar ']'
    return a

parseJsonArray :: Parser String
parseJsonArray = do
    _ <- parseAnd (parseChar '[') (parseMany (parseAnyChar " \n\t"))
    a <- concat <$> parseMany (parseJsonStringQuote <|> parseJsonInt <|>
        parseJsonArray <|> parseJsonObject)
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseChar ']'
    _ <- parseMany (parseAnyChar " \n\t,")
    return a

parseJsonObject :: Parser String
parseJsonObject = do
    _ <- parseAnd (parseChar '{') (parseMany (parseAnyChar " \n\t"))
    a <- concat <$> parseMany (parseJsonSection <|> parseJsonBold <|>
        parseJsonItalic <|> parseJsonCode <|> parseJsonCodeBlock <|>
        parseJsonLink <|> parseJsonImage <|> parseJsonList)
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseChar '}'
    _ <- parseMany (parseAnyChar " \n\t,")
    return a

parseJsonSection :: Parser String
parseJsonSection = do
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseString "\"section\":"
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseChar '{'
    _ <- parseMany (parseAnyChar " \n\t")
    a <- concat <$> parseMany (parseJsonTitle <|>
        parseJsonContent <|> parseJsonStringQuote)
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseChar '}'
    return a

parseJsonTitle :: Parser String
parseJsonTitle = do
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseString "\"title\":"
    _ <- parseMany (parseAnyChar " \n\t")
    a <- parseStringQuote
    _ <- parseMany (parseAnyChar " \n\t,")
    return a

parseJsonContent :: Parser String
parseJsonContent = do
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseString "\"content\":"
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseChar '['
    _ <- parseMany (parseAnyChar " \n\t")
    a <- concat <$> parseMany (parseJsonStringQuote <|> parseJsonInt <|>
        parseJsonArray <|> parseJsonObject)
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseChar ']'
    return a

parseJsonBold :: Parser String
parseJsonBold = do
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseString "\"bold\":"
    _ <- parseMany (parseAnyChar " \n\t")
    a <- parseStringQuote
    _ <- parseMany (parseAnyChar " \n\t,")
    return a

parseJsonItalic :: Parser String
parseJsonItalic = do
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseString "\"italic\":"
    _ <- parseMany (parseAnyChar " \n\t")
    a <- parseStringQuote
    _ <- parseMany (parseAnyChar " \n\t,")
    return a

parseJsonCode :: Parser String
parseJsonCode = do
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseString "\"code\":"
    _ <- parseMany (parseAnyChar " \n\t")
    a <- parseStringQuote
    _ <- parseMany (parseAnyChar " \n\t,")
    return a

parseJsonCodeBlock :: Parser String
parseJsonCodeBlock = do
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseString "\"codeblock\":"
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseChar '['
    _ <- parseMany (parseAnyChar " \n\t")
    a <- concat <$> parseMany parseJsonArray
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseChar ']'
    return a

parseJsonList :: Parser String
parseJsonList = do
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseString "\"list\":"
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseChar '['
    _ <- parseMany (parseAnyChar " \n\t")
    a <- concat <$> parseMany parseJsonArray
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseChar ']'
    _ <- parseMany (parseAnyChar " \n\t,")
    return a

parseJsonUrl :: Parser String
parseJsonUrl = do
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseString "\"url\":"
    _ <- parseMany (parseAnyChar " \n\t")
    a <- parseStringQuote
    _ <- parseMany (parseAnyChar " \n\t,")
    return a

parseJsonImage :: Parser String
parseJsonImage = do
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseString "\"image\":"
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseChar '{'
    _ <- parseMany (parseAnyChar " \n\t")
    a <- concat <$> parseMany (parseJsonUrl <|> parseJsonAlt)
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseChar '}'
    return a

parseJsonAlt :: Parser String
parseJsonAlt = do
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseString "\"alt\":"
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseChar '['
    _ <- parseMany (parseAnyChar " \n\t")
    a <- parseStringQuote
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseChar ']'
    return a

parseJsonLink :: Parser String
parseJsonLink = do
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseString "\"link\":"
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseChar '{'
    _ <- parseMany (parseAnyChar " \n\t")
    a <- concat <$> parseMany (parseJsonUrl <|> parseJsonContent)
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseChar '}'
    return a

parseJsonStringQuote :: Parser String
parseJsonStringQuote = do
    _ <- parseMany (parseAnyChar " \n\t")
    a <- parseStringQuote
    _ <- parseMany (parseAnyChar " \n\t,")
    return a

parseJsonInt :: Parser String
parseJsonInt = do
    _ <- parseMany (parseAnyChar " \n\t")
    a <- parseIntString
    _ <- parseMany (parseAnyChar " \n\t,")
    return a

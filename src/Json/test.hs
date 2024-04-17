


import ParsingLib


data JsonValue = JsonArray | JsonNull | JsonBool | JsonNumber | JsonString deriving (Show, Eq)

parseJsonArray :: Parser String
parseJsonArray = do
    _ <- parseAnd (parseChar '[') (parseMany (parseAnyChar " \n\t"))
    a <- parseStringQuote <|> parseIntString <|> parseJsonArray <|> parseJsonObject
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseChar ']'
    return a

parseJsonObject :: Parser String
parseJsonObject = do
    _ <- parseAnd (parseChar '{') (parseMany (parseAnyChar " \n\t"))
    a <- parseStringQuote
    _ <- parseMany (parseAnyChar " \n\t")
    _ <- parseChar '}'
    return a

parseSpaces :: Parser String
parseSpaces = parseMany (parseAnyChar " \t\n\r")

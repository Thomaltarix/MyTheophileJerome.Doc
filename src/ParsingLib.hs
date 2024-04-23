{-
-- EPITECH PROJECT, 2023
-- MyPandoc
-- File description:
-- ParsingLib
-}

module ParsingLib (
    Parser(..),
    parseChar,
    parseAnyChar,
    parseAnyCharNotMatch,
    parseOr,
    parseAnd,
    parseAndWith,
    parseMany,
    parseSome,
    parseUntil,
    parseUInt,
    parseInt,
    parseString,
    (<|>),
    parseStringQuote,
    parseStringBalise,
    parseIntString
    ) where

import Control.Applicative
import Text.Read ( readMaybe )
import Data.List ( stripPrefix )

data Parser a = Parser {
    runParser :: String -> Maybe (a, String)
}

instance Functor Parser where
    fmap fct (Parser p) = Parser q where
        q str = case p str of
            Just (x, rest) -> Just (fct x, rest)
            Nothing        -> Nothing

instance Applicative Parser where
    pure x = Parser $ \str -> Just (x, str)
    (Parser p1) <*> (Parser p2) = Parser $ \str ->
        case p1 str of
            Just (f, rest) ->
                case p2 rest of
                    Just (x, rest') -> Just (f x, rest')
                    Nothing         -> Nothing
            Nothing         -> Nothing

instance Alternative Parser where
    empty = Parser $ const Nothing
    (Parser p1) <|> (Parser p2) = Parser $ \str ->
        case p1 str of
            Just (f, rest) -> Just(f, rest)
            Nothing ->
                case p2 str of
                    Just (x, rest') -> Just (x, rest')
                    Nothing         -> Nothing

instance Monad Parser where
    return = pure
    (Parser p1) >>= f = Parser $ \str ->
        case p1 str of
            Just (x, rest) -> runParser (f x) rest
            Nothing        -> Nothing
    (>>) = (*>)

parseChar :: Char -> Parser Char
parseChar c = Parser p where
    p (x:xs)
        | x == c = Just (c, xs)
        | otherwise = Nothing
    p _ = Nothing

parseAnyChar :: String -> Parser Char
parseAnyChar (c:cs) = Parser p where
    p (x:xs)
        | c == x = Just(c, xs)
        | otherwise = runParser (parseAnyChar cs) (x:xs)
    p _ = Nothing
parseAnyChar _ = Parser p where
    p _ = Nothing

parseAnyCharNotMatch :: String -> Parser Char
parseAnyCharNotMatch str = Parser p where
    p (x:xs)
        | x `elem` str = Nothing
        | otherwise = Just (x, xs)
    p [] = Nothing

parseOr :: Parser a -> Parser a -> Parser a
parseOr parser1 parser2 = Parser p where
    p str = case runParser parser1 str of
        Just result -> Just result
        Nothing     -> runParser parser2 str

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd parser1 parser2 = Parser p where
    p (x:xs) = case runParser parser1 (x:xs) of
        Just (c, _) -> case runParser parser2 xs of
            Just (c1, r) -> Just ((c, c1), r)
            Nothing      -> Nothing
        Nothing     -> Nothing
    p [] = Nothing

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith ftc parser1 parser2 = Parser p where
    p (x:xs) = case runParser parser1 (x:xs) of
        Just (c, _) -> case runParser parser2 xs of
            Just (c1, r) -> Just (ftc c c1, r)
            Nothing      -> Nothing
        Nothing     -> Nothing
    p [] = Nothing

parseMany :: Parser a -> Parser [a]
parseMany parser = Parser p where
    p str = case runParser parser str of
        Just (a, str') -> case runParser (parseMany parser) str' of
            Just (as, str'') -> Just (a:as, str'')
            Nothing -> Just ([a], str')
        Nothing -> Just ([], str)

parseSome :: Parser a -> Parser [a]
parseSome parser = Parser p where
    p str = case runParser parser str of
        Just (a, str') -> case runParser (parseSome parser) str' of
            Just (as, str'') -> Just (a:as, str'')
            Nothing -> Just ([a], str')
        Nothing -> Nothing

parseUInt :: Parser Int
parseUInt = Parser p where
    p str = do
        (int, str') <- runParser (parseMany (parseAnyChar ['0'..'9'])) str
        case readMaybe int of
            Just n -> Just (n, str')
            Nothing -> Nothing

parseInt :: Parser Int
parseInt = Parser p where
    p ('-':str) = case runParser parseUInt str of
        Just (nb, str') -> Just (-nb, str')
        Nothing      -> Nothing
    p (x:str) = runParser parseUInt (x:str)
    p [] = Nothing

isSameString :: String -> String -> Bool
isSameString [] _ = True
isSameString _ [] = False
isSameString (x:xs) (y:ys) = x == y && isSameString xs ys

parseString :: String -> Parser String
parseString strToMatch = Parser p where
    p str
        | isSameString strToMatch str =
            Just (strToMatch, drop (length strToMatch) str)
        | otherwise = Nothing

parseStringQuote :: Parser String
parseStringQuote = Parser p where
    p ('\"':str) = case break (=='\"') str of
        (quoted, rest) -> do
            Just (quoted, drop 1 rest)
    p _ = Nothing

parseStringBalise :: String -> Parser String
parseStringBalise balise = Parser p where
    p ('<':rest) = case stripPrefix balise rest of
        Just rest' -> case stripPrefix ">" rest' of
            Just content -> case break (== '<') content of
                (balised, str) -> Just (balised, drop (3 + length balise) str)
            Nothing -> Nothing
        Nothing -> Nothing
    p _ = Nothing

parseUIntString :: Parser String
parseUIntString = Parser p where
    p str = do
        (int, str') <- runParser (parseMany (parseAnyChar ['0'..'9'])) str
        if int == "" then Nothing else Just (int, str')

parseIntString :: Parser String
parseIntString = Parser p where
    p ('-':str) = case runParser parseUIntString str of
        Just (nb, str') -> Just ('-':nb, str')
        Nothing      -> Nothing
    p (x:str) = runParser parseUIntString (x:str)
    p [] = Nothing

parseUntil :: Char -> Parser String
parseUntil c = Parser p where
    p str = case break (==c) str of
        (quoted, rest) -> Just (quoted, drop 1 rest)
    p _ = Nothing

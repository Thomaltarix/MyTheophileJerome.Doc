{-
-- EPITECH PROJECT, 2023
-- MyPandoc
-- File description:
-- ParseContent
-}
module Markdown.ParseContent (
    getContent
    ) where

import ParsingLib

import DataStructure

import Markdown.ParseConcatHeaders

import Markdown.ParseParagraph()

getContent :: String -> (Maybe Object, String)
getContent str = case runParser parseMdBody str of
    Nothing -> (Nothing, str)
    Just (c, str') -> (Just c, str')

parseMdBody :: Parser Object
parseMdBody = do
    a <- concatList 0
    return (createObject ListT (Just "body") a)

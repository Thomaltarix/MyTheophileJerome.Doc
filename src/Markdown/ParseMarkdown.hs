{-
-- EPITECH PROJECT, 2023
-- MyPandoc
-- File description:
-- ParseMarkdown
-}

module Markdown.ParseMarkdown (
    markdownParsing
    ) where

import Markdown.ParseHeader(getHeader)
import Markdown.ParseContent
import DataStructure

testmd :: IO ()
testmd = readFile "tests/syntaxe.md" >>= print . markdownParsing

markdownParsing :: String -> Maybe Header
markdownParsing str =
    case getHeader str defaultHeader of
        (Just h, str') -> Just h
        -- (Just h, str') -> case getContent str' of
        --     (Just c, _) -> Just DataStruct {
        --         header = h,
        --         content = c
        --         }
        --     (Nothing, _) -> Nothing
        (Nothing, _) -> Nothing

{-
-- EPITECH PROJECT, 2024
-- MyTh-ophileJ-r-me.Doc
-- File description:
-- markdownDIsplay
-}

module Markdown.MarkdownDisplay (
    getMarkdownObjectTag,
    getMarkdownDataTag,
    printMarkdown
    ) where

import System.IO
import DataStructure

getMarkdownObjectTag :: Object -> (String, String)
getMarkdownObjectTag obj = case objType obj of
    SectionT -> ("#", "")
    ListT -> ("", "")
    CodeBlockT -> ("```", "```")
    LinkT -> ("[", "]")
    ParagraphT -> ("", "")
    ImageT -> ("![", "]")

getMarkdownDataTag :: Data -> (String, String)
getMarkdownDataTag data_ = case dataType data_ of
    TextT -> ("", "")
    ItalicT -> ("*", "*")
    BoldT -> ("**", "**")
    CodeT -> ("`", "`")

printMarkdown :: Maybe Handle -> DataStruct -> IO ()
printMarkdown _ _ = putStrLn "MARKDOWN"

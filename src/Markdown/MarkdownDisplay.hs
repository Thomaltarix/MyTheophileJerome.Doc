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
import PrintString

getMarkdownObjectTag :: Object -> (String, String)
getMarkdownObjectTag obj = case objType obj of
    SectionT -> ("#", "")
    ListT -> ("", "")
    CodeBlockT -> ("```", "```")
    LinkT -> ("[", "]")
    ImageT -> ("![", "]")
    AltT -> ("", "")

getMarkdownDataTag :: Data -> (String, String)
getMarkdownDataTag data_ = case dataType data_ of
    TextT -> ("", "")
    ItalicT -> ("*", "*")
    BoldT -> ("**", "**")
    CodeT -> ("`", "`")

printMarkdownSymbol :: Maybe Handle -> String -> Int -> IO ()
printMarkdownSymbol handle "" spaces = printString handle "" spaces
printMarkdownSymbol handle symbol_ spaces =
    printString handle symbol_ 0 >>
    printString handle ": " 0

printMarkdownData :: Maybe Handle -> Data -> Bool -> Int -> IO ()
printMarkdownData handle data_ end spaces =
    let (startTag, endTag) = getMarkdownDataTag data_ in
    printMarkdownSymbol handle (myFromJustString (symbol data_)) spaces >>
    printString handle startTag 0 >>
    printString handle (myFromJustString (dataContent data_)) 0 >>
    printString handle endTag 0 >>
    printEnd handle end

printMarkdownHeaderData :: Maybe Handle -> Maybe Data -> Bool -> Int -> IO ()
printMarkdownHeaderData _ Nothing _ _ = return ()
printMarkdownHeaderData handle (Just data_) end spaces =
    printMarkdownData handle data_ end spaces

printMarkdownHeader :: Maybe Handle -> Header -> Bool -> Int -> IO ()
printMarkdownHeader handle
    Header {title = title_, author = author_, date = date_} end spaces =
    let (_, endTag) = getMarkdownObjectTag (Object SectionT Nothing []) in
    printString handle "---\n" spaces >>
    printMarkdownHeaderData handle title_ True spaces >>
    printMarkdownHeaderData handle author_ True spaces >>
    printMarkdownHeaderData handle date_ True spaces >>
    printString handle "---\n" spaces

printMarkdown :: Maybe Handle -> DataStruct -> IO ()
printMarkdown handle dataStruct =
    let (startTag, endTag) = getMarkdownObjectTag (Object SectionT Nothing []) in
    printMarkdownHeader handle (header dataStruct) False 0 >>
    -- printJsonObject handle (content dataStruct) True 4 >>
    printString handle endTag 0 >>
    printString handle "\n" 0
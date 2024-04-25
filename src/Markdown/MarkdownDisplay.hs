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
import Control.Monad
import Control.Exception (handle)

getMarkdownObjectTag :: Object -> (String, String)
getMarkdownObjectTag obj = case objType obj of
    SectionT -> ("# ", "")
    ListT -> ("", "")
    CodeBlockT -> ("```\n", "```")
    LinkT -> ("[", "]")
    ImageT -> ("![", "]")
    AltT -> ("", "")
    ParagraphT -> ("", "")

getMarkdownDataTag :: Data -> (String, String)
getMarkdownDataTag data_ = case dataType data_ of
    TextT -> ("", "")
    ItalicT -> ("*", "*")
    BoldT -> ("**", "**")
    CodeT -> ("`", "`")

printMarkdownSymbol :: Maybe Handle -> String -> Int -> IO ()
printMarkdownSymbol handle "" spaces = printString handle "" spaces
printMarkdownSymbol handle symbol_ _ =
    printString handle symbol_ 0 >>
    printString handle ": " 0

printMarkdownDataHeader :: Maybe Handle -> Data -> Bool -> Int -> IO ()
printMarkdownDataHeader handle data_ end spaces =
    let (startTag, endTag) = getMarkdownDataTag data_ in
    printMarkdownSymbol handle (myFromJustString (symbol data_)) spaces >>
    printString handle startTag 0 >>
    printString handle (myFromJustString (dataContent data_)) 0 >>
    printString handle endTag 0 >>
    printEnd handle end

printMarkdownHeaderData :: Maybe Handle -> Maybe Data -> Bool -> Int -> IO ()
printMarkdownHeaderData _ Nothing _ _ = return ()
printMarkdownHeaderData handle (Just data_) end spaces =
    printMarkdownDataHeader handle data_ end spaces

printMarkdownHeader :: Maybe Handle -> Header -> Bool -> Int -> IO ()
printMarkdownHeader handle
    Header {title = title_, author = author_, date = date_} _ spaces =
    printString handle "---\n" spaces >>
    printMarkdownHeaderData handle title_ True spaces >>
    printMarkdownHeaderData handle author_ True spaces >>
    printMarkdownHeaderData handle date_ True spaces >>
    printString handle "---\n\n" spaces

printMarkdownData :: Maybe Handle -> Data -> Int -> IO ()
printMarkdownData handle data_ spaces =
    let (startTag, endTag) = getMarkdownDataTag data_ in
    printString handle startTag 0 >>
    printString handle (myFromJustString (dataContent data_)) 0 >>
    when (symbol data_ == Just "title") (printString handle "\n\n" 0) >>
    printString handle endTag 0

printMarkdownContent :: Maybe Handle -> [Either Data Object] -> Int -> IO ()
printMarkdownContent _ [] _ = return ()
printMarkdownContent handle [x] spaces = case x of
    Left data_ -> printMarkdownData handle data_ spaces
    Right obj -> printMarkdownObject handle obj spaces
    >> printMarkdownContent handle [] spaces
printMarkdownContent handle (x:xs) spaces = case x of
    Left data_ -> printMarkdownData handle data_ spaces
    Right obj -> printMarkdownObject handle obj spaces
    >> printMarkdownContent handle xs spaces

printEndSection :: Maybe Handle -> Object -> String -> IO ()
printEndSection handle obj endTag = let data_ = datas obj in
    case [data_] of
        [[Left (Data _ _ (Just "bold"))]] -> printString handle endTag 0
        [[Left (Data _ _ (Just "italic"))]] -> printString handle endTag 0
        [[Left (Data _ _ (Just "code"))]] -> printString handle endTag 0
        _ -> printString handle (endTag ++ "\n\n") 0

printMarkdownObject :: Maybe Handle -> Object -> Int -> IO ()
printMarkdownObject handle obj spaces =
    let (startTag, endTag) = getMarkdownObjectTag obj in
    case objSymbol obj of
        Nothing ->  printMarkdownContent handle (datas obj) 0 >>
                    printEndSection handle obj endTag
        Just _ ->   printString handle startTag 0 >>
                    printMarkdownContent handle (datas obj) 0 >>
                    printEndSection handle obj endTag

printMarkdown :: Maybe Handle -> DataStruct -> IO ()
printMarkdown handle dataStruct =
    let (_, endTag) = getMarkdownObjectTag (Object SectionT Nothing []) in
    printMarkdownHeader handle (header dataStruct) False 0 >>
    printMarkdownObject handle (content dataStruct) 0 >>
    printString handle endTag 0 >>
    printString handle "\n" 0
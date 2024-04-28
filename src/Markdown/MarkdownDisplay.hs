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

import System.IO ( Handle )
import DataStructure
    ( Data(..),
    Object(..),
    DataType(CodeT, TextT, ItalicT, BoldT),
    ObjectType(SectionT, LinkT, ImageT, AltT, ParagraphT, ListT,
    CodeBlockT),
    Header(..),
    DataStruct(content, header) )
import PrintString ( myFromJustString, printEnd, printString )
import Control.Monad ( when )

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

printListSymbol :: Maybe Handle -> Data -> Bool -> IO ()
printListSymbol handle data_ list = when list $ printString handle "- " 0

printMarkdownData :: Maybe Handle -> Data -> Bool -> IO ()
printMarkdownData handle data_ list =
    let (startTag, endTag) = getMarkdownDataTag data_ in
    printString handle startTag 0 >>
    printListSymbol handle data_ list >>
    printString handle (myFromJustString (dataContent data_)) 0 >>
    when (symbol data_ == Just "title" && dataContent data_ /= Just "")
        (printString handle "\n\n" 0) >>
    printString handle endTag 0

printMarkdownContent :: Maybe Handle -> [Either Data Object] -> Int -> Bool -> Int -> IO ()
printMarkdownContent _ [] _ _ _ = return ()
-- printMarkdownContent handle [x] doubleReturn list = case x of
--     Left data_ -> printMarkdownData handle data_ list
--     Right obj -> printMarkdownObject handle obj doubleReturn list
--     >> printMarkdownContent handle [] doubleReturn list
printMarkdownContent handle_ (x:xs) nbReturn list nbSection = case x of
    Left data_ -> printMarkdownData handle_ data_ list
    Right obj -> printMarkdownObject handle_ obj nbReturn list nbSection
    >> printMarkdownContent handle_ xs nbReturn list nbSection

displayReturns :: Maybe Handle -> Int -> IO ()
displayReturns _ 0 = print 0
displayReturns handle_ nb = print nb >> printString handle_ "\n" 0
    >> displayReturns handle_ (nb - 1)

printEndSection :: Maybe Handle -> Object -> String -> Int -> IO ()
printEndSection handle_ obj endTag nbReturn = let data_ = datas obj in
   case [data_] of
       [[Left (Data _ _ (Just "bold"))]] -> printString handle_ endTag 0
       [[Left (Data _ _ (Just "italic"))]] -> printString handle_ endTag 0
       [[Left (Data _ _ (Just "code"))]] -> printString handle_ endTag 0
       _ -> displayReturns handle_ nbReturn >> printString handle_ endTag 0

calcNbReturnHelper :: [Either Data Object] -> Int
calcNbReturnHelper [] = 2
calcNbReturnHelper (Left _:x) = calcNbReturnHelper x
calcNbReturnHelper (Right xs:_) = case objSymbol xs of
    Just "link" -> 1
    Just "image" -> 1
    Just "list" -> 1
    _ -> calcNbReturnHelper (datas xs)

calcNbReturn :: Int -> Object -> Int
calcNbReturn 1 _ = 0
calcNbReturn 0 _ = 1
calcNbReturn _ obj = case objType obj of
    CodeBlockT -> 0
    _ -> calcNbReturnHelper (datas obj)

isList :: Bool -> Object -> Bool
isList True _ = True
isList False obj = case objType obj of
    ListT -> case objSymbol obj of
        Just "list" -> True
        _ -> False
    _ -> False

getUrl :: [Either Data Object] -> String
getUrl [] = ""
getUrl (Left data_:x) = if symbol data_ == Just "url"
                                then myFromJustString (dataContent data_)
                                else getUrl x
getUrl (Right _:x) = getUrl x

getContentUrl :: [Either Data Object] -> String
getContentUrl [] = ""
getContentUrl (Left data_:x) = if symbol data_ == Nothing
                                then myFromJustString (dataContent data_)
                                else getContentUrl x
getContentUrl (Right xs:x) = if objSymbol xs == Just "content"
                                then getContentUrl (datas xs)
                                else getContentUrl x

printMarkdownLink :: Maybe Handle -> Object -> IO ()
printMarkdownLink handle_ obj =
    let url = getUrl (datas obj)
        content_ = getContentUrl (datas obj) in
    printString handle_ "[" 0 >>
    printString handle_ content_ 0 >>
    printString handle_ "](" 0 >>
    printString handle_ url 0 >>
    printString handle_ ")" 0

getContentImage :: [Either Data Object] -> String
getContentImage [] = ""
getContentImage (Left data_@Data {symbol = Nothing}:_) =
    myFromJustString (dataContent data_)
getContentImage (Left _:xs) = getContentImage xs
getContentImage (Right obj@Object {objSymbol = Just "alt"}:_) =
    getContentImage (datas obj)
getContentImage (Right _:xs) = getContentImage xs

printMarkdownImage :: Maybe Handle -> Object -> IO ()
printMarkdownImage handle_ obj =
    let url = getUrl (datas obj)
        content_ = getContentImage (datas obj) in
    printString handle_ "![" 0 >>
    printString handle_ content_ 0 >>
    printString handle_ "](" 0 >>
    printString handle_ url 0 >>
    printString handle_ ")" 0

needPrintStartTag :: [Either Data Object] -> Bool
needPrintStartTag [] = False
needPrintStartTag (Left data_:_) = symbol data_ == Just "title"
    && dataContent data_ /= Just ""
needPrintStartTag (Right _:_) = False

printStartTag :: Maybe Handle -> Object -> Int -> Int -> IO ()
printStartTag handle_ obj@(Object {objType = CodeBlockT}) _ _ =
        printString handle_ "```\n" 0
printStartTag handle_ obj _ nbSection =
    when (needPrintStartTag (datas obj)) $ printString handle_
        (replicate nbSection '#') 0
        >> printString handle_ " " 0

printMarkdownObject :: Maybe Handle -> Object -> Int -> Bool -> Int -> IO ()
printMarkdownObject handle obj nbReturn list nbSection =
    let (_, endTag) = getMarkdownObjectTag obj in
    case objSymbol obj of
        Nothing ->  printMarkdownContent handle (datas obj)
            (calcNbReturn nbReturn obj) (isList list obj) nbSection >>
            printEndSection handle obj endTag (calcNbReturn nbReturn obj)
        Just "link" -> printMarkdownLink handle obj
        Just "image" -> printMarkdownImage handle obj
        Just "section" -> printStartTag handle obj 0 nbSection >>
                          printMarkdownContent handle (datas obj)
                            (calcNbReturn nbReturn obj) (isList list obj)
                            (nbSection + 1) >>
                          printEndSection handle obj endTag
                            (calcNbReturn nbReturn obj)
        Just _ ->   printStartTag handle obj 0 nbSection >>
                    printMarkdownContent handle (datas obj)
                        (calcNbReturn nbReturn obj) (isList list obj) nbSection
                    >> printEndSection handle obj endTag
                        (calcNbReturn nbReturn obj)

printMarkdown :: Maybe Handle -> DataStruct -> IO ()
printMarkdown handle dataStruct =
    let (_, endTag) = getMarkdownObjectTag (Object SectionT Nothing []) in
    printMarkdownHeader handle (header dataStruct) True 0 >>
    printMarkdownObject handle (content dataStruct) 2 False 1 >>
    printString handle endTag 0 >>
    printString handle "\n" 0

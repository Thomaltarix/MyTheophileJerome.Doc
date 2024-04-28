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

printMarkdownSymbol :: Maybe Handle -> String  -> IO ()
printMarkdownSymbol handle "" = printString handle ""
printMarkdownSymbol handle symbol_ =
    printString handle symbol_ >>
    printString handle ": "

printMarkdownDataHeader :: Maybe Handle -> Data -> Bool  -> IO ()
printMarkdownDataHeader handle data_ end =
    let (startTag, endTag) = getMarkdownDataTag data_ in
    printMarkdownSymbol handle (myFromJustString (symbol data_)) >>
    printString handle startTag >>
    printString handle (myFromJustString (dataContent data_)) >>
    printString handle endTag >>
    printString handle "\n" >>
    printEnd handle end

printMarkdownHeaderData :: Maybe Handle -> Maybe Data -> IO ()
printMarkdownHeaderData _ Nothing = return ()
printMarkdownHeaderData handle (Just data_) =
    printMarkdownDataHeader handle data_ True

printMarkdownHeader :: Maybe Handle -> Header -> Bool -> IO ()
printMarkdownHeader handle
    Header {title = title_, author = author_, date = date_} _ =
    printString handle "---\n" >>
    printMarkdownHeaderData handle title_ >>
    printMarkdownHeaderData handle author_ >>
    printMarkdownHeaderData handle date_ >>
    printString handle "---\n\n"

printListSymbol :: Maybe Handle -> Data -> Bool -> IO ()
printListSymbol handle _ list = when list $ printString handle "- "

printMarkdownData :: Maybe Handle -> Data -> Bool -> IO ()
printMarkdownData handle data_ list =
    let (startTag, endTag) = getMarkdownDataTag data_ in
    printString handle startTag >>
    printListSymbol handle data_ list >>
    printString handle (myFromJustString (dataContent data_)) >>
    when (symbol data_ == Just "title" && dataContent data_ /= Just "")
        (printString handle "\n\n") >>
    printString handle endTag

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
displayReturns _ 0 = return ()
displayReturns handle_ nb = printString handle_ "\n"
    >> displayReturns handle_ (nb - 1)

printEndSection :: Maybe Handle -> Object -> String -> Int -> IO ()
printEndSection handle_ obj endTag nbReturn = let data_ = datas obj in
    case [data_] of
        [[Left (Data _ _ (Just "bold"))]] -> printString handle_ endTag
        [[Left (Data _ _ (Just "italic"))]] -> printString handle_ endTag
        [[Left (Data _ _ (Just "code"))]] -> printString handle_ endTag
        _ -> displayReturns handle_ nbReturn >> printString handle_ endTag

calcNbReturnHelper :: [Either Data Object] -> Int
calcNbReturnHelper [] = 2
calcNbReturnHelper (Left _:x) = calcNbReturnHelper x
calcNbReturnHelper (Right Object {objSymbol = Just "link"}:_) = 1
calcNbReturnHelper (Right Object {objSymbol = Just "image"}:_) = 1
calcNbReturnHelper (Right Object {objSymbol = Just "list"}:_) = 1
calcNbReturnHelper (Right x:_) = calcNbReturnHelper (datas x)

calcNbReturn :: Int -> Object -> Int
calcNbReturn 1 _ = 0
calcNbReturn 0 _ = 1
calcNbReturn _ Object {objType = CodeBlockT} = 0
calcNbReturn _ obj = calcNbReturnHelper (datas obj)

isList :: Bool -> Object -> Bool
isList True _ = True
isList False Object {objType = ListT, objSymbol = Just "list"} = True
isList _ _ = False

getUrl :: [Either Data Object] -> String
getUrl [] = ""
getUrl (Left data_@Data {symbol = Just "url"}:_) =
    myFromJustString (dataContent data_)
getUrl (Left _:xs) = getUrl xs
getUrl (Right _:x) = getUrl x

getContentUrl :: [Either Data Object] -> String
getContentUrl [] = ""
getContentUrl (Left data_@Data {symbol = Nothing}:_) =
    myFromJustString (dataContent data_)
getContentUrl (Left _:xs) = getContentUrl xs
getContentUrl (Right obj@Object {objSymbol = Just "content"}:_) =
    getContentUrl (datas obj)
getContentUrl (Right _:xs) = getContentUrl xs

printMarkdownLink :: Maybe Handle -> Object -> IO ()
printMarkdownLink handle_ obj =
    let url = getUrl (datas obj)
        content_ = getContentUrl (datas obj) in
    printString handle_ "[" >>
    printString handle_ content_ >>
    printString handle_ "](" >>
    printString handle_ url >>
    printString handle_ ")"

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
    printString handle_ "![" >>
    printString handle_ content_ >>
    printString handle_ "](" >>
    printString handle_ url >>
    printString handle_ ")"

needPrintStartTag :: [Either Data Object] -> Bool
needPrintStartTag [] = False
needPrintStartTag (Left data_:_) = symbol data_ == Just "title"
    && dataContent data_ /= Just ""
needPrintStartTag (Right _:_) = False

printStartTag :: Maybe Handle -> Object -> Int -> Int -> IO ()
printStartTag handle_ Object {objType = CodeBlockT} _ _ =
        printString handle_ "```\n"
printStartTag handle_ obj _ nbSection =
    when (needPrintStartTag (datas obj)) $ printString handle_
        (replicate nbSection '#')
        >> printString handle_ " "

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
    printMarkdownHeader handle (header dataStruct) True>>
    printMarkdownObject handle (content dataStruct) 2 False 1 >>
    printString handle endTag >>
    printString handle "\n"

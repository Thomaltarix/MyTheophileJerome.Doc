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
import PrintString

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

getMarkdownSymbol :: String -> String
getMarkdownSymbol "" = ""
getMarkdownSymbol symbol_ = symbol_ ++ ": "

getMarkdownDataHeader :: Data -> Bool -> String
getMarkdownDataHeader data_ end =
    let (startTag, endTag) = getMarkdownDataTag data_ in
    getMarkdownSymbol (myFromJustString (symbol data_)) ++
    startTag ++
    myFromJustString (dataContent data_) ++
    endTag ++
    getEnd end

getMakrdownHeaderData :: Maybe Data -> String
getMakrdownHeaderData Nothing = ""
getMakrdownHeaderData (Just data_) = getMarkdownDataHeader data_ True

printMarkdownHeader :: Header -> Bool -> String
printMarkdownHeader Header {title = title_, author = author_, date = date_} _ =
    "---\n" ++
    getMakrdownHeaderData title_ ++
    getMakrdownHeaderData author_ ++
    getMakrdownHeaderData date_ ++
    "---\n\n"

getListSymbol ::Bool -> String
getListSymbol False = ""
getListSymbol True = "- "

getMarkdownData :: Data -> Bool -> String
getMarkdownData data_ list =
    let (startTag, endTag) = getMarkdownDataTag data_ in
    startTag ++
    getListSymbol list ++
    myFromJustString (dataContent data_) ++
    (if symbol data_ == Just "title" && dataContent data_ /= Just ""
        then "\n\n" else "") ++
    endTag

getMarkdownContent :: [Either Data Object] -> Int -> Bool -> Int -> String
getMarkdownContent [] _ _ _ = ""
getMarkdownContent [x] doubleReturn list nbSection = case x of
    Left data_ -> getMarkdownData data_ list
    Right obj -> getMarkdownObject obj doubleReturn list nbSection
getMarkdownContent (x:xs) nbReturn list nbSection =
    getMarkdownContent [x] nbReturn list nbSection ++
    getMarkdownContent xs nbReturn list nbSection

printEndSection :: Object -> String -> String
printEndSection obj endTag = let data_ = datas obj in
    case [data_] of
        [[Left (Data _ _ (Just "bold"))]] -> endTag
        [[Left (Data _ _ (Just "italic"))]] -> endTag
        [[Left (Data _ _ (Just "code"))]] -> endTag
        _ -> endTag

calcNbReturnHelper :: [Either Data Object] -> Int
calcNbReturnHelper [] = 0
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

getMarkdownLink :: Object -> String
getMarkdownLink obj =
    let url = getUrl (datas obj)
        content_ = getContentUrl (datas obj) in
    "[" ++ content_ ++ "](" ++ url ++ ")"

getContentImage :: [Either Data Object] -> String
getContentImage [] = ""
getContentImage (Left data_@Data {symbol = Nothing}:_) =
    myFromJustString (dataContent data_)
getContentImage (Left _:xs) = getContentImage xs
getContentImage (Right obj@Object {objSymbol = Just "alt"}:_) =
    getContentImage (datas obj)
getContentImage (Right _:xs) = getContentImage xs

getMarkdownImage :: Object -> String
getMarkdownImage obj =
    let url = getUrl (datas obj)
        content_ = getContentImage (datas obj) in
    "![" ++ content_ ++ "](" ++ url ++ ")"

needPrintStartTag :: [Either Data Object] -> Bool
needPrintStartTag [] = False
needPrintStartTag (Left data_:_) = symbol data_ == Just "title"
    && dataContent data_ /= Just ""
needPrintStartTag (Right _:_) = False

getStartTag :: Object -> Int -> Int -> String
getStartTag Object {objType = CodeBlockT} _ _ = "```\n"
getStartTag obj _ nbSection = if needPrintStartTag (datas obj)
    then replicate nbSection '#' ++ " " else ""

handleMarkdownObject :: Object -> Int -> Bool -> Int -> String -> String
handleMarkdownObject obj@Object {objType = ListT, objSymbol = Nothing} _ list nbSection _ =
    getMarkdownContent (datas obj) 0 list nbSection ++ (if list then "\n" else "\n\n")
handleMarkdownObject obj@(Object {objSymbol = Just "link"}) _ _ _ _ =
    getMarkdownLink obj
handleMarkdownObject obj@(Object {objSymbol = Just "image"}) _ _ _ _ =
    getMarkdownImage obj
handleMarkdownObject
    obj@(Object {objSymbol = Nothing}) nReturn list nSection end =
        getMarkdownContent (datas obj) (calcNbReturn nReturn obj)
        (isList list obj) nSection ++
        printEndSection obj end
handleMarkdownObject
    obj@(Object {objSymbol = Just "section"}) nReturn list nSection end =
        getStartTag obj 0 nSection ++
        getMarkdownContent (datas obj) (calcNbReturn nReturn obj)
        (isList list obj) (nSection + 1) ++
        printEndSection obj end
handleMarkdownObject obj nReturn list nSection end =
    getStartTag obj 0 nSection ++
    getMarkdownContent (datas obj) (calcNbReturn nReturn obj)
    (isList list obj) nSection ++
    printEndSection obj end

getMarkdownObject :: Object -> Int -> Bool -> Int -> String
getMarkdownObject obj nbReturn list nbSection =
    let (_, endTag) = getMarkdownObjectTag obj in
    handleMarkdownObject obj nbReturn list nbSection endTag

printMarkdown :: Maybe Handle -> DataStruct -> IO ()
printMarkdown handle dataStruct =
    let (_, endTag) = getMarkdownObjectTag (Object SectionT Nothing []) in
    let markdown = printMarkdownHeader (header dataStruct) True ++
            getMarkdownObject (content dataStruct) 2 False 1 ++
            endTag ++ "\n" in
    printString handle markdown

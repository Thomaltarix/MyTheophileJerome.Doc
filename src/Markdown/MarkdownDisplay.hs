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

printMarkdownDataHeader :: Data -> Bool -> String
printMarkdownDataHeader data_ end =
    let (startTag, endTag) = getMarkdownDataTag data_ in
    getMarkdownSymbol (myFromJustString (symbol data_)) ++
    startTag ++
    myFromJustString (dataContent data_) ++
    endTag ++
    getEnd end

printMakrdownHeaderData :: Maybe Data -> String
printMakrdownHeaderData Nothing = ""
printMakrdownHeaderData (Just data_) = printMarkdownDataHeader data_ True

printMarkdownHeader :: Header -> Bool -> String
printMarkdownHeader Header {title = title_, author = author_, date = date_} _ =
    "---\n" ++
    printMakrdownHeaderData title_ ++
    printMakrdownHeaderData author_ ++
    printMakrdownHeaderData date_ ++
    "---\n\n"

printListSymbol ::Bool -> String
printListSymbol False = ""
printListSymbol True = "- "

printMarkdownData :: Data -> Bool -> String
printMarkdownData data_ list =
    let (startTag, endTag) = getMarkdownDataTag data_ in
    startTag ++
    printListSymbol list ++
    myFromJustString (dataContent data_) ++
    (if symbol data_ == Just "title" && dataContent data_ /= Just ""
        then "\n\n" else "") ++
    endTag

printMarkdownContent :: [Either Data Object] -> Int -> Bool -> Int -> String
printMarkdownContent [] _ _ _ = ""
printMarkdownContent [x] doubleReturn list nbSection = case x of
    Left data_ -> printMarkdownData data_ list
    Right obj -> printMarkdownObject obj doubleReturn list nbSection
printMarkdownContent (x:xs) nbReturn list nbSection =
    printMarkdownContent [x] nbReturn list nbSection ++
    printMarkdownContent xs nbReturn list nbSection

displayReturns :: Int -> String
displayReturns 0 = ""
displayReturns nb = "\n" ++ displayReturns (nb - 1)

printEndSection :: Object -> String -> Int -> String
printEndSection obj endTag nbReturn = let data_ = datas obj in
    case [data_] of
        [[Left (Data _ _ (Just "bold"))]] -> endTag
        [[Left (Data _ _ (Just "italic"))]] -> endTag
        [[Left (Data _ _ (Just "code"))]] -> endTag
        _ -> displayReturns nbReturn ++ endTag

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

printMarkdownLink :: Object -> String
printMarkdownLink obj =
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

printMarkdownImage :: Object -> String
printMarkdownImage obj =
    let url = getUrl (datas obj)
        content_ = getContentImage (datas obj) in
    "![" ++ content_ ++ "](" ++ url ++ ")"

needPrintStartTag :: [Either Data Object] -> Bool
needPrintStartTag [] = False
needPrintStartTag (Left data_:_) = symbol data_ == Just "title"
    && dataContent data_ /= Just ""
needPrintStartTag (Right _:_) = False

printStartTag :: Object -> Int -> Int -> String
printStartTag Object {objType = CodeBlockT} _ _ = "```\n"
printStartTag obj _ nbSection = if needPrintStartTag (datas obj)
    then replicate nbSection '#' ++ " " else ""

printMarkdownObject :: Object -> Int -> Bool -> Int -> String
printMarkdownObject obj nbReturn list nbSection =
    let (_, endTag) = getMarkdownObjectTag obj in
    case objSymbol obj of
        Nothing ->  printMarkdownContent (datas obj)
            (calcNbReturn nbReturn obj) (isList list obj) nbSection ++
            printEndSection obj endTag (calcNbReturn nbReturn obj)
        Just "link" -> printMarkdownLink obj
        Just "image" -> printMarkdownImage obj
        Just "section" -> printStartTag obj 0 nbSection ++
                        printMarkdownContent (datas obj)
                            (calcNbReturn nbReturn obj) (isList list obj)
                            (nbSection + 1) ++
                        printEndSection obj endTag
                            (calcNbReturn nbReturn obj)
        Just _ ->   printStartTag obj 0 nbSection ++
                    printMarkdownContent (datas obj)
                        (calcNbReturn nbReturn obj) (isList list obj) nbSection
                    ++ printEndSection obj endTag
                        (calcNbReturn nbReturn obj)

printMarkdown :: Maybe Handle -> DataStruct -> IO ()
printMarkdown handle dataStruct =
    let (_, endTag) = getMarkdownObjectTag (Object SectionT Nothing []) in
    let markdown = printMarkdownHeader (header dataStruct) True ++
            printMarkdownObject (content dataStruct) 2 False 1 ++
            endTag ++ "\n" in
    printString handle markdown

{-
-- EPITECH PROJECT, 2024
-- MyTh-ophileJ-r-me.Doc
-- File description:
-- xmlDisplay
-}

module Xml.XmlDisplay (
    getXmlObjectTag,
    getXmlDataTag,
    printXml
    ) where

import System.IO
import DataStructure
import PrintString

data XmlTypes = DocumentT | HeaderT | BodyT deriving Eq

getXmlTag :: XmlTypes -> (String, String)
getXmlTag DocumentT = ("<document>\n", "</document>\n")
getXmlTag HeaderT = ("<header", "</header>\n")
getXmlTag BodyT = ("<body>\n", "</body>\n")

getXmlObjectTag :: Object -> (String, String)
getXmlObjectTag obj = case objType obj of
    SectionT -> ("<section", "</section>\n")
    ListT -> ("<list>\n", "</list>\n")
    CodeBlockT -> ("<codeblock>\n", "</codeblock>\n")
    LinkT -> ("<link", "</link>\n")
    ParagraphT -> ("<paragraph", "</paragraph>\n")
    ImageT -> ("<image", "</image>\n")
    AltT -> ("", "")

getXmlDataTag :: Data -> (String, String)
getXmlDataTag data_ = case dataType data_ of
    TextT -> ("<paragraph>", "</paragraph>\n")
    ItalicT -> ("<italic>", "</italic>")
    BoldT -> ("<bold>", "</bold>")
    CodeT -> ("<code>", "</code>")

printXmlStartSymbol :: Maybe Handle -> String -> Int -> IO ()
printXmlStartSymbol handle "" spaces = printString handle "" spaces
printXmlStartSymbol handle symbol_ spaces =
    printString handle "<" spaces >>
    printString handle symbol_ 0 >>
    printString handle ">" 0

printXmlEndSymbol :: Maybe Handle -> String -> Int -> IO ()
printXmlEndSymbol handle "" spaces = printString handle "\n" spaces
printXmlEndSymbol handle symbol_ _ =
    printString handle "</" 0 >>
    printString handle symbol_ 0 >>
    printString handle ">\n" 0

printXmlData :: Maybe Handle -> Data -> Int -> Bool -> IO ()
printXmlData _ Data {symbol = Just "title"} _ _ = return ()
printXmlData handle data_@(Data {dataType = ItalicT}) _ _ =
    let (startTag, endTag) = getXmlDataTag data_ in
    printString handle startTag 0 >>
    printString handle (myFromJustString (dataContent data_)) 0 >>
    printString handle endTag 0
printXmlData handle data_@(Data {dataType = BoldT}) _ _ =
    let (startTag, endTag) = getXmlDataTag data_ in
    printString handle startTag 0 >>
    printString handle (myFromJustString (dataContent data_)) 0 >>
    printString handle endTag 0
printXmlData handle data_@(Data {dataType = CodeT}) _ _ =
    let (startTag, endTag) = getXmlDataTag data_ in
    printString handle startTag 0 >>
    printString handle (myFromJustString (dataContent data_)) 0 >>
    printString handle endTag 0
printXmlData handle data_@(Data {dataType = TextT}) spaces _ =
    let (startTag, endTag) = getXmlDataTag data_ in
    printString handle startTag spaces >>
    printString handle (myFromJustString (dataContent data_)) 0 >>
    printString handle endTag 0

printXmlSpecialData :: Maybe Handle -> [Either Data Object] -> Int -> String -> IO ()
printXmlSpecialData _ [] _ _ = return ()
printXmlSpecialData handle [x] _ symbol_ = case x of
    Left data_ ->   if symbol_ == myFromJustString (symbol data_) then
                        printString handle " " 0 >>
                        printString handle symbol_ 0 >>
                        printString handle "=\"" 0 >>
                        printString handle (myFromJustString (dataContent data_)) 0 >>
                        printString handle "\">\n" 0
                    else return ()
    Right _ -> return ()
printXmlSpecialData handle (x:xs) spaces symbol_ = case x of
    Left _ -> printXmlSpecialData handle [x] spaces symbol_
    Right _ -> return ()
    >> printXmlSpecialData handle xs spaces symbol_
printXmlObjectStartSymbol :: Maybe Handle -> Object -> Int -> IO Bool
printXmlObjectStartSymbol handle
    obj@(Object {objType = SectionT, objSymbol = Just "section"}) spaces =
    let (startTag, _) = getXmlObjectTag obj in
    printString handle startTag spaces >>
    printXmlSpecialData handle (datas obj) 0 "title" >>
    return True
printXmlObjectStartSymbol handle
    obj@(Object {objType = ListT, objSymbol = Just "list"}) spaces =
    let (startTag, _) = getXmlObjectTag obj in
    printString handle startTag spaces >>
    return True
printXmlObjectStartSymbol handle
    obj@(Object {objType = CodeBlockT, objSymbol = Just "codeblock"}) spaces =
    let (startTag, _) = getXmlObjectTag obj in
    printString handle startTag spaces >>
    return True
printXmlObjectStartSymbol handle
    obj@(Object {objType = LinkT, objSymbol = Just "link"}) spaces =
    let (startTag, _) = getXmlObjectTag obj in
    printString handle startTag spaces >>
    printXmlSpecialData handle (datas obj) 0 "url" >>
    return False
printXmlObjectStartSymbol handle
    obj@(Object {objType = ImageT, objSymbol = Just "image"}) spaces =
    let (startTag, _) = getXmlObjectTag obj in
    printString handle startTag spaces >>
    printXmlSpecialData handle (datas obj) 0 "url" >>
    return False
printXmlObjectStartSymbol handle obj@(Object {objType = ParagraphT}) spaces =
    let (startTag, _) = getXmlObjectTag obj in
    printString handle startTag (spaces - 4) >>
    return True
printXmlObjectStartSymbol _ _ _ = return False

printXmlObjectEndSymbol :: Maybe Handle -> Object -> Int -> IO ()
printXmlObjectEndSymbol handle
    obj@(Object {objType = SectionT, objSymbol = Just "section"}) spaces =
    let (_, endTag) = getXmlObjectTag obj in
    printString handle endTag spaces
printXmlObjectEndSymbol handle
    obj@(Object {objType = ListT, objSymbol = Just "list"}) spaces =
    let (_, endTag) = getXmlObjectTag obj in
    printString handle endTag spaces
printXmlObjectEndSymbol handle
    obj@(Object {objType = CodeBlockT, objSymbol = Just "codeblock"}) spaces =
    let (_, endTag) = getXmlObjectTag obj in
    printString handle endTag spaces
printXmlObjectEndSymbol handle
    obj@(Object {objType = LinkT, objSymbol = Just "link"}) spaces =
    let (_, endTag) = getXmlObjectTag obj in
    printString handle endTag spaces
printXmlObjectEndSymbol handle
    obj@(Object {objType = ImageT, objSymbol = Just "image"}) spaces =
    let (_, endTag) = getXmlObjectTag obj in
    printString handle endTag spaces
printXmlObjectEndSymbol _ _ _ = return ()

printXmlObject :: Maybe Handle -> Object -> Int -> IO ()
printXmlObject handle obj spaces = do
    needSpaces <- printXmlObjectStartSymbol handle obj spaces
    if needSpaces then
        printXmlContent handle (datas obj) (spaces + 4) >>
        printXmlObjectEndSymbol handle obj spaces
    else printXmlContent handle (datas obj) spaces >>
        printXmlObjectEndSymbol handle obj spaces

printXmlHeaderData :: Maybe Handle -> Maybe Data -> Int -> IO ()
printXmlHeaderData _ Nothing _ = return ()
printXmlHeaderData handle (Just data_@(Data {symbol = Just "title"})) _ =
    printString handle (myFromJustString (dataContent data_)) 0
printXmlHeaderData handle (Just data_) spaces =
    printXmlStartSymbol handle (myFromJustString (symbol data_)) spaces >>
    printString handle (myFromJustString (dataContent data_)) 0 >>
    printXmlEndSymbol handle (myFromJustString (symbol data_)) 0

printXmlHeader :: Maybe Handle -> Header -> Int -> IO ()
printXmlHeader handle header_ spaces =
    let (startTag, endTag) = getXmlTag HeaderT in
    printString handle startTag spaces >>
    printString handle " title=\"" 0 >>
    printXmlHeaderData handle (title header_) 0 >>
    printString handle "\">\n" 0 >>
    printXmlHeaderData handle (author header_) (spaces + 4) >>
    printXmlHeaderData handle (date header_) (spaces + 4) >>
    printString handle endTag spaces

printXml :: Maybe Handle -> DataStruct -> IO ()
printXml _ _ = putStrLn "XML"
    printXmlHeader handle (header dataStruct) 4 >>

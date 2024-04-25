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

data XmlIndentation = NoSpaces | NormalSpaces | FourSpaces deriving Eq

getXmlTag :: XmlTypes -> (String, String)
getXmlTag DocumentT = ("<document>\n", "</document>\n")
getXmlTag HeaderT = ("<header", "</header>\n")
getXmlTag BodyT = ("<body>\n", "</body>\n")

getXmlObjectTag :: Object -> (String, String)
getXmlObjectTag obj = case objType obj of
    SectionT -> ("<section", "</section>\n")
    ListT -> ("<list>\n", "</list>\n")
    CodeBlockT -> ("<codeblock>\n", "</codeblock>\n")
    LinkT -> ("<link", "</link>")
    ParagraphT -> ("<paragraph>", "</paragraph>\n")
    ImageT -> ("<image", "</image>")
    AltT -> ("", "")

getXmlDataTag :: Data -> (String, String)
getXmlDataTag data_ = case dataType data_ of
    TextT -> ("", "")
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

printXmlData :: Maybe Handle -> Data -> Int -> IO ()
printXmlData _ Data {symbol = Just "title"} _ = return ()
printXmlData _ Data {symbol = Just "url"} _ = return ()
printXmlData handle data_@(Data {dataType = ItalicT}) _ =
    let (startTag, endTag) = getXmlDataTag data_ in
    printString handle startTag 0 >>
    printString handle (myFromJustString (dataContent data_)) 0 >>
    printString handle endTag 0
printXmlData handle data_@(Data {dataType = BoldT}) _ =
    let (startTag, endTag) = getXmlDataTag data_ in
    printString handle startTag 0 >>
    printString handle (myFromJustString (dataContent data_)) 0 >>
    printString handle endTag 0
printXmlData handle data_@(Data {dataType = CodeT}) _ =
    let (startTag, endTag) = getXmlDataTag data_ in
    printString handle startTag 0 >>
    printString handle (myFromJustString (dataContent data_)) 0 >>
    printString handle endTag 0
printXmlData handle data_@(Data {dataType = TextT}) spaces =
    let (startTag, endTag) = getXmlDataTag data_ in
    printString handle startTag spaces >>
    printString handle (myFromJustString (dataContent data_)) 0 >>
    printString handle endTag 0

printXmlSpecialData :: Maybe Handle -> [Either Data Object] -> String -> Bool -> IO ()
printXmlSpecialData _ [] _ _ = return ()
printXmlSpecialData handle [Left data_] symbol_ True =
    printString handle " " 0 >>
    printString handle symbol_ 0 >>
    printString handle "=\"" 0 >>
    printString handle (myFromJustString (dataContent data_)) 0 >>
    printString handle "\">\n" 0
printXmlSpecialData handle [Left data_] symbol_ False =
    printString handle " " 0 >>
    printString handle symbol_ 0 >>
    printString handle "=\"" 0 >>
    printString handle (myFromJustString (dataContent data_)) 0 >>
    printString handle "\">" 0
printXmlSpecialData handle (x:xs) symbol_ enter = case x of
    Left _ -> printXmlSpecialData handle [x] symbol_ enter
    Right _ -> return ()
    >> printXmlSpecialData handle xs symbol_ enter

printXmlContent :: Maybe Handle -> [Either Data Object] -> Int -> IO ()
printXmlContent _ [] _ = return ()
printXmlContent handle [Left data_] spaces =
    printXmlData handle data_ spaces
printXmlContent handle [Right obj] spaces = printXmlObject handle obj spaces
printXmlContent handle (Left data_:xs) spaces =
    printXmlData handle data_ spaces >>
    printXmlContent handle xs spaces
printXmlContent handle (Right obj:xs) spaces = printXmlObject handle obj spaces
    >> printXmlContent handle xs spaces

needUrl :: Maybe Handle -> Object -> Int -> IO XmlIndentation
needUrl handle obj spaces =
    let (startTag, _) = getXmlObjectTag obj in
    printString handle startTag spaces >>
    printXmlSpecialData handle (datas obj) "url" False >>
    return NoSpaces

needTitle :: Maybe Handle -> Object -> Int -> IO XmlIndentation
needTitle handle obj spaces =
    let (startTag, _) = getXmlObjectTag obj in
    printString handle startTag spaces >>
    printXmlSpecialData handle (datas obj) "title" True >>
    return FourSpaces

needNothing :: Maybe Handle -> Object -> Int -> IO XmlIndentation
needNothing handle obj spaces =
    let (startTag, _) = getXmlObjectTag obj in
    printString handle startTag spaces >>
    return FourSpaces

needNoSpaces :: Maybe Handle -> Object -> Int -> IO XmlIndentation
needNoSpaces handle obj spaces =
    let (startTag, _) = getXmlObjectTag obj in
    printString handle startTag spaces >>
    return NoSpaces

printXmlObjectStartSymbol :: Maybe Handle -> Object -> Int -> IO XmlIndentation
printXmlObjectStartSymbol handle
    obj@Object {objType = SectionT, objSymbol = Just "section"} spaces =
        needTitle handle obj spaces
printXmlObjectStartSymbol handle
    obj@Object {objType = ListT, objSymbol = Just "list"} spaces =
        needNothing handle obj spaces
printXmlObjectStartSymbol handle
    obj@Object {objType = CodeBlockT, objSymbol = Just "codeblock"} spaces =
        needNothing handle obj spaces
printXmlObjectStartSymbol handle
    obj@Object {objType = LinkT, objSymbol = Just "link"} spaces =
        needUrl handle obj spaces
printXmlObjectStartSymbol handle
    obj@Object {objType = ImageT, objSymbol = Just "image"} spaces =
        needUrl handle obj spaces
printXmlObjectStartSymbol handle
    obj@Object {objType = ParagraphT} spaces =
        needNoSpaces handle obj spaces
printXmlObjectStartSymbol _ _ _ = return NormalSpaces

displayEnd :: Maybe Handle -> Object -> Int -> IO ()
displayEnd handle obj spaces =
    let (_, endTag) = getXmlObjectTag obj in
    printString handle endTag spaces

printXmlObjectEndSymbol :: Maybe Handle -> Object -> Int -> IO ()
printXmlObjectEndSymbol handle
    obj@Object {objType = SectionT, objSymbol = Just "section"} spaces =
        displayEnd handle obj spaces
printXmlObjectEndSymbol handle
    obj@Object {objType = ListT, objSymbol = Just "list"} spaces =
        displayEnd handle obj spaces
printXmlObjectEndSymbol handle
    obj@Object {objType = CodeBlockT, objSymbol = Just "codeblock"} spaces =
        displayEnd handle obj spaces
printXmlObjectEndSymbol handle
    obj@Object {objType = LinkT, objSymbol = Just "link"} spaces =
        displayEnd handle obj spaces
printXmlObjectEndSymbol handle
    obj@Object {objType = ImageT, objSymbol = Just "image"} spaces =
        displayEnd handle obj spaces
printXmlObjectEndSymbol handle
    obj@Object {objType = ParagraphT} spaces =
        displayEnd handle obj spaces
printXmlObjectEndSymbol _ _ _ = return ()

checkSubObject :: [Either Data Object] -> Bool
checkSubObject [] = True
checkSubObject (Left _:xs) = checkSubObject xs
checkSubObject (Right _:_) = False

isAData :: Either Data Object -> Bool
isAData (Left (Data {dataType = TextT})) = True
isAData (Right Object {objType = CodeBlockT}) = False
isAData (Right (Object {objType = SectionT})) = False
isAData (Right (Object {objType = ListT})) = False
isAData (Right (Object {objType = ImageT})) = False
isAData (Right (Object {objType = LinkT})) = False
isAData (Right obj) = checkSubObject (datas obj)
isAData _ = False

checkDatas :: [Either Data Object] -> Bool
checkDatas [] = False
checkDatas [x] = isAData x
checkDatas (Right (Object {objType = SectionT}):xs) = checkDatas xs
checkDatas (x:xs) = isAData x && checkDatas xs

checkObject :: Object -> Object
checkObject obj@(Object {objType = ListT})
    | checkDatas (datas obj) = obj {objType = ParagraphT}
    | otherwise = obj
checkObject obj = obj

handleSpaces :: Maybe Handle -> Object -> Int -> XmlIndentation -> IO ()
handleSpaces handle obj spaces NormalSpaces =
    printXmlContent handle (datas obj) spaces >>
    printXmlObjectEndSymbol handle obj spaces
handleSpaces handle obj spaces FourSpaces =
    printXmlContent handle (datas obj) (spaces + 4) >>
    printXmlObjectEndSymbol handle obj spaces
handleSpaces handle obj _ NoSpaces =
    printXmlContent handle (datas obj) 0 >>
    printXmlObjectEndSymbol handle obj 0

printXmlObject :: Maybe Handle -> Object -> Int -> IO ()
printXmlObject handle obj spaces = do
    let newObj = checkObject obj
    needSpaces <- printXmlObjectStartSymbol handle newObj spaces
    handleSpaces handle newObj spaces needSpaces

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

printXmlBody :: Maybe Handle -> Object -> Int -> IO ()
printXmlBody handle obj spaces =
    let (startTag, endTag) = getXmlTag BodyT in
    printString handle startTag spaces >>
    printXmlContent handle (datas obj) (spaces + 4) >>
    printString handle endTag spaces

printXml :: Maybe Handle -> DataStruct -> IO ()
printXml handle dataStruct =
    let (startTag, endTag) = getXmlTag DocumentT in
    printString handle startTag 0 >>
    printXmlHeader handle (header dataStruct) 4 >>
    printXmlBody handle (content dataStruct) 4 >>
    printString handle endTag 0 >>
    printString handle "\n" 0

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

printXmlStartSymbol :: String -> Int -> String
printXmlStartSymbol "" spaces = getString "" spaces
printXmlStartSymbol symbol_ spaces =
    getString "<" spaces ++
    getString symbol_ 0 ++
    getString ">" 0

printXmlEndSymbol :: String -> Int -> String
printXmlEndSymbol "" spaces = getString "\n" spaces
printXmlEndSymbol symbol_ _ =
    getString "</" 0 ++
    getString symbol_ 0 ++
    getString ">\n" 0

printXmlData :: Data -> Int -> String
printXmlData Data {symbol = Just "title"} _ = ""
printXmlData Data {symbol = Just "url"} _ = ""
printXmlData data_@(Data {dataType = TextT}) spaces =
    let (startTag, endTag) = getXmlDataTag data_ in
    getString startTag spaces ++
    getString (myFromJustString (dataContent data_)) 0 ++
    getString endTag 0
printXmlData data_ _ =
    let (startTag, endTag) = getXmlDataTag data_ in
    getString startTag 0 ++
    getString (myFromJustString (dataContent data_)) 0 ++
    getString endTag 0

getEndOfSection :: Bool -> String
getEndOfSection False = "\n"
getEndOfSection True = ""

printXmlSpecialData :: [Either Data Object] -> String -> Bool -> String
printXmlSpecialData [Left data_] symbol_ end =
    getString " " 0 ++
    getString symbol_ 0 ++
    getString "=\"" 0 ++
    getString (myFromJustString (dataContent data_)) 0 ++
    getString "\">" 0 ++
    getEndOfSection end
printXmlSpecialData (Left data_:xs) symbol_ end =
    printXmlSpecialData [Left data_] symbol_ end ++
    printXmlSpecialData xs symbol_ end
printXmlSpecialData _ _ _ = ""

printXmlContent :: [Either Data Object] -> Int -> String
printXmlContent [] _ = ""
printXmlContent [Left data_] spaces = printXmlData data_ spaces
printXmlContent [Right obj] spaces = printXmlObject obj spaces
printXmlContent (Left data_:xs) spaces =
    printXmlData data_ spaces ++
    printXmlContent xs spaces
printXmlContent (Right obj:xs) spaces = printXmlObject obj spaces
    ++ printXmlContent xs spaces

needUrl :: Object -> Int -> (String, XmlIndentation)
needUrl obj spaces =
    let (startTag, _) = getXmlObjectTag obj in
    (getString startTag spaces
    ++ printXmlSpecialData (datas obj) "url" True, NoSpaces)

needTitle :: Object -> Int -> (String, XmlIndentation)
needTitle obj spaces =
    let (startTag, _) = getXmlObjectTag obj in
    (getString startTag spaces
    ++ printXmlSpecialData (datas obj) "title" False, FourSpaces)

needNothing :: Object -> Int -> (String, XmlIndentation)
needNothing obj spaces =
    let (startTag, _) = getXmlObjectTag obj in
    (getString startTag spaces, FourSpaces)

needNoSpaces :: Object -> Int -> (String, XmlIndentation)
needNoSpaces obj spaces =
    let (startTag, _) = getXmlObjectTag obj in
    (getString startTag spaces, NoSpaces)

printXmlObjectStartSymbol :: Object -> Int -> (String, XmlIndentation)
printXmlObjectStartSymbol
    obj@Object {objType = SectionT, objSymbol = Just "section"} spaces =
    needTitle obj spaces
printXmlObjectStartSymbol
    obj@Object {objType = ListT, objSymbol = Just "list"} spaces =
    needNothing obj spaces
printXmlObjectStartSymbol
    obj@Object {objType = CodeBlockT, objSymbol = Just "codeblock"} spaces =
    needNothing obj spaces
printXmlObjectStartSymbol
    obj@Object {objType = LinkT, objSymbol = Just "link"} spaces =
    needUrl obj spaces
printXmlObjectStartSymbol
    obj@Object {objType = ImageT, objSymbol = Just "image"} spaces =
    needUrl obj spaces
printXmlObjectStartSymbol obj@Object {objType = ParagraphT} spaces =
    needNoSpaces obj spaces
printXmlObjectStartSymbol _ _ = ("", NormalSpaces)

displayEnd :: Object -> Int -> String
displayEnd obj spaces =
    let (_, endTag) = getXmlObjectTag obj in
    getString endTag spaces

printXmlObjectEndSymbol :: Object -> Int -> String
printXmlObjectEndSymbol
    obj@Object {objType = SectionT, objSymbol = Just "section"} spaces =
    displayEnd obj spaces
printXmlObjectEndSymbol
    obj@Object {objType = ListT, objSymbol = Just "list"} spaces =
    displayEnd obj spaces
printXmlObjectEndSymbol
    obj@Object {objType = CodeBlockT, objSymbol = Just "codeblock"} spaces =
    displayEnd obj spaces
printXmlObjectEndSymbol
    obj@Object {objType = LinkT, objSymbol = Just "link"} spaces =
    displayEnd obj spaces
printXmlObjectEndSymbol
    obj@Object {objType = ImageT, objSymbol = Just "image"} spaces =
    displayEnd obj spaces
printXmlObjectEndSymbol
    obj@Object {objType = ParagraphT} spaces =
    displayEnd obj spaces
printXmlObjectEndSymbol _ _ = ""

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

handleSpaces :: Object -> Int -> XmlIndentation -> String
handleSpaces obj spaces NormalSpaces = printXmlContent (datas obj) spaces
    ++ printXmlObjectEndSymbol obj spaces
handleSpaces obj spaces FourSpaces = printXmlContent (datas obj) (spaces + 4)
    ++ printXmlObjectEndSymbol obj spaces
handleSpaces obj _ NoSpaces = printXmlContent (datas obj) 0
    ++ printXmlObjectEndSymbol obj 0

printXmlObject :: Object -> Int -> String
printXmlObject
    obj@(Object {objType = ListT, objSymbol = Just "content"}) spaces =
    let (str, indent) = printXmlObjectStartSymbol obj spaces in
    str ++ handleSpaces obj spaces indent
printXmlObject obj spaces =
    let newObj = checkObject obj in
    let (str, indent) = printXmlObjectStartSymbol newObj spaces in
    str ++ handleSpaces newObj spaces indent

printXmlHeaderData :: Maybe Data -> Int -> String
printXmlHeaderData Nothing _ = ""
printXmlHeaderData (Just data_@(Data {symbol = Just "title"})) _ =
    getString (myFromJustString (dataContent data_)) 0
printXmlHeaderData (Just data_) spaces =
    printXmlStartSymbol (myFromJustString (symbol data_)) spaces
    ++ getString (myFromJustString (dataContent data_)) 0
    ++ printXmlEndSymbol (myFromJustString (symbol data_)) 0

printXmlHeader :: Header -> Int -> String
printXmlHeader header_ spaces =
    let (startTag, endTag) = getXmlTag HeaderT in
    getString startTag spaces
    ++ getString " title=\"" 0
    ++ printXmlHeaderData (title header_) 0
    ++ getString "\">\n" 0
    ++ printXmlHeaderData (author header_) (spaces + 4)
    ++ printXmlHeaderData (date header_) (spaces + 4)
    ++ getString endTag spaces

printXmlBody :: Object -> Int -> String
printXmlBody obj spaces =
    let (startTag, endTag) = getXmlTag BodyT in
    getString startTag spaces
    ++ printXmlContent (datas obj) (spaces + 4)
    ++ getString endTag spaces

printXml :: Maybe Handle -> DataStruct -> IO ()
printXml handle dataStruct =
    let (startTag, endTag) = getXmlTag DocumentT in
    let xml = startTag ++ printXmlHeader (header dataStruct) 4
            ++ printXmlBody (content dataStruct) 4 ++ endTag ++ "\n" in
        printString handle xml

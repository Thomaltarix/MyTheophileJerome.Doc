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

getXmlStartSymbol :: String -> Int -> String
getXmlStartSymbol "" spaces = getString "" spaces
getXmlStartSymbol symbol_ spaces =
    getString "<" spaces ++
    getString symbol_ 0 ++
    getString ">" 0

getXmlEndSymbol :: String -> Int -> String
getXmlEndSymbol "" spaces = getString "\n" spaces
getXmlEndSymbol symbol_ _ =
    getString "</" 0 ++
    getString symbol_ 0 ++
    getString ">\n" 0

getXmlData :: Data -> Int -> String
getXmlData Data {symbol = Just "title"} _ = ""
getXmlData Data {symbol = Just "url"} _ = ""
getXmlData data_@(Data {dataType = TextT}) spaces =
    let (startTag, endTag) = getXmlDataTag data_ in
    getString startTag spaces ++
    getString (myFromJustString (dataContent data_)) 0 ++
    getString endTag 0
getXmlData data_ _ =
    let (startTag, endTag) = getXmlDataTag data_ in
    getString startTag 0 ++
    getString (myFromJustString (dataContent data_)) 0 ++
    getString endTag 0

getEndOfSection :: Bool -> String
getEndOfSection False = "\n"
getEndOfSection True = ""

getXmlSpecialData :: [Either Data Object] -> String -> Bool -> String
getXmlSpecialData [Left data_] symbol_ end =
    getString " " 0 ++
    getString symbol_ 0 ++
    getString "=\"" 0 ++
    getString (myFromJustString (dataContent data_)) 0 ++
    getString "\">" 0 ++
    getEndOfSection end
getXmlSpecialData (Left data_:xs) symbol_ end =
    getXmlSpecialData [Left data_] symbol_ end ++
    getXmlSpecialData xs symbol_ end
getXmlSpecialData _ _ _ = ""

getXmlContent :: [Either Data Object] -> Int -> String
getXmlContent [] _ = ""
getXmlContent [Left data_] spaces = getXmlData data_ spaces
getXmlContent [Right obj] spaces = getXmlObject obj spaces
getXmlContent (Left data_:xs) spaces =
    getXmlData data_ spaces ++
    getXmlContent xs spaces
getXmlContent (Right obj:xs) spaces = getXmlObject obj spaces
    ++ getXmlContent xs spaces

needUrl :: Object -> Int -> (String, XmlIndentation)
needUrl obj spaces =
    let (startTag, _) = getXmlObjectTag obj in
    (getString startTag spaces
    ++ getXmlSpecialData (datas obj) "url" True, NoSpaces)

needTitle :: Object -> Int -> (String, XmlIndentation)
needTitle obj spaces =
    let (startTag, _) = getXmlObjectTag obj in
    (getString startTag spaces
    ++ getXmlSpecialData (datas obj) "title" False, FourSpaces)

needNothing :: Object -> Int -> (String, XmlIndentation)
needNothing obj spaces =
    let (startTag, _) = getXmlObjectTag obj in
    (getString startTag spaces, FourSpaces)

needNoSpaces :: Object -> Int -> (String, XmlIndentation)
needNoSpaces obj spaces =
    let (startTag, _) = getXmlObjectTag obj in
    (getString startTag spaces, NoSpaces)

getXmlObjectStartSymbol :: Object -> Int -> (String, XmlIndentation)
getXmlObjectStartSymbol
    obj@Object {objType = SectionT, objSymbol = Just "section"} spaces =
    needTitle obj spaces
getXmlObjectStartSymbol
    obj@Object {objType = ListT, objSymbol = Just "list"} spaces =
    needNothing obj spaces
getXmlObjectStartSymbol
    obj@Object {objType = CodeBlockT, objSymbol = Just "codeblock"} spaces =
    needNothing obj spaces
getXmlObjectStartSymbol
    obj@Object {objType = LinkT, objSymbol = Just "link"} spaces =
    needUrl obj spaces
getXmlObjectStartSymbol
    obj@Object {objType = ImageT, objSymbol = Just "image"} spaces =
    needUrl obj spaces
getXmlObjectStartSymbol obj@Object {objType = ParagraphT} spaces =
    needNoSpaces obj spaces
getXmlObjectStartSymbol _ _ = ("", NormalSpaces)

displayEnd :: Object -> Int -> String
displayEnd obj spaces =
    let (_, endTag) = getXmlObjectTag obj in
    getString endTag spaces

getXmlObjectEndSymbol :: Object -> Int -> String
getXmlObjectEndSymbol
    obj@Object {objType = SectionT, objSymbol = Just "section"} spaces =
    displayEnd obj spaces
getXmlObjectEndSymbol
    obj@Object {objType = ListT, objSymbol = Just "list"} spaces =
    displayEnd obj spaces
getXmlObjectEndSymbol
    obj@Object {objType = CodeBlockT, objSymbol = Just "codeblock"} spaces =
    displayEnd obj spaces
getXmlObjectEndSymbol
    obj@Object {objType = LinkT, objSymbol = Just "link"} spaces =
    displayEnd obj spaces
getXmlObjectEndSymbol
    obj@Object {objType = ImageT, objSymbol = Just "image"} spaces =
    displayEnd obj spaces
getXmlObjectEndSymbol
    obj@Object {objType = ParagraphT} spaces =
    displayEnd obj spaces
getXmlObjectEndSymbol _ _ = ""

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
handleSpaces obj spaces NormalSpaces = getXmlContent (datas obj) spaces
    ++ getXmlObjectEndSymbol obj spaces
handleSpaces obj spaces FourSpaces = getXmlContent (datas obj) (spaces + 4)
    ++ getXmlObjectEndSymbol obj spaces
handleSpaces obj _ NoSpaces = getXmlContent (datas obj) 0
    ++ getXmlObjectEndSymbol obj 0

getXmlObject :: Object -> Int -> String
getXmlObject
    obj@(Object {objType = ListT, objSymbol = Just "content"}) spaces =
    let (str, indent) = getXmlObjectStartSymbol obj spaces in
    str ++ handleSpaces obj spaces indent
getXmlObject obj spaces =
    let newObj = checkObject obj in
    let (str, indent) = getXmlObjectStartSymbol newObj spaces in
    str ++ handleSpaces newObj spaces indent

getXmlHeaderData :: Maybe Data -> Int -> String
getXmlHeaderData Nothing _ = ""
getXmlHeaderData (Just data_@(Data {symbol = Just "title"})) _ =
    getString (myFromJustString (dataContent data_)) 0
getXmlHeaderData (Just data_) spaces =
    getXmlStartSymbol (myFromJustString (symbol data_)) spaces
    ++ getString (myFromJustString (dataContent data_)) 0
    ++ getXmlEndSymbol (myFromJustString (symbol data_)) 0

getXmlHeader :: Header -> Int -> String
getXmlHeader header_ spaces =
    let (startTag, endTag) = getXmlTag HeaderT in
    getString startTag spaces
    ++ getString " title=\"" 0
    ++ getXmlHeaderData (title header_) 0
    ++ getString "\">\n" 0
    ++ getXmlHeaderData (author header_) (spaces + 4)
    ++ getXmlHeaderData (date header_) (spaces + 4)
    ++ getString endTag spaces

getXmlBody :: Object -> Int -> String
getXmlBody obj spaces =
    let (startTag, endTag) = getXmlTag BodyT in
    getString startTag spaces
    ++ getXmlContent (datas obj) (spaces + 4)
    ++ getString endTag spaces

printXml :: Maybe Handle -> DataStruct -> IO ()
printXml handle dataStruct =
    let (startTag, endTag) = getXmlTag DocumentT in
    let xml = startTag ++ getXmlHeader (header dataStruct) 4
            ++ getXmlBody (content dataStruct) 4 ++ endTag in
        printString handle xml

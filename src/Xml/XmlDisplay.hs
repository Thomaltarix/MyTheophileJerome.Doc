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

getXmlObjectTag :: Object -> (String, String)
getXmlObjectTag obj = case objType obj of
    SectionT -> ("<section>", "</section>")
    ListT -> ("<list>", "</list>")
    CodeBlockT -> ("<codeblock>", "</codeblock>")
    LinkT -> ("<link>", "</link>")
    ImageT -> ("<image>", "</image>")
    AltT -> ("<alt>", "</alt>")

getXmlDataTag :: Data -> (String, String)
getXmlDataTag data_ = case dataType data_ of
    TextT -> ("", "")
    ItalicT -> ("<italic>", "</italic>")
    BoldT -> ("<bold>", "</bold>")
    CodeT -> ("<code>", "</code>")

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

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
    AltT -> ("", "")

getXmlDataTag :: Data -> (String, String)
getXmlDataTag data_ = case dataType data_ of
    TextT -> ("", "")
    ItalicT -> ("<italic>", "</italic>")
    BoldT -> ("<bold>", "</bold>")
    CodeT -> ("<code>", "</code>")


printXml :: Maybe Handle -> DataStruct -> IO ()
printXml _ _ = putStrLn "XML"

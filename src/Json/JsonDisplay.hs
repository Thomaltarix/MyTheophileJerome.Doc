{-
-- EPITECH PROJECT, 2024
-- MyTh-ophileJ-r-me.Doc
-- File description:
-- jsonDisplay
-}

module Json.JsonDisplay (
    getJsonObjectTag,
    getJsonDataTag,
    printJson
    ) where

import System.IO
import DataStructure
import PrintString
import Data.Maybe

getJsonObjectTag :: Object -> (String, String)
getJsonObjectTag obj = case objType obj of
    SectionT -> ("{\n", "}")
    ListT -> ("[\n", "]")
    CodeBlockT -> ("[\n", "]")
    LinkT -> ("{\n", "}")
    ParagraphT -> ("[\n", "]")
    ImageT -> ("{\n", "}")
    AltT -> ("[\n", "]")

getJsonDataTag :: Data -> (String, String)
getJsonDataTag data_ = case dataType data_ of
    TextT -> ("\"", "\"")
    ItalicT -> ("\"", "\"")
    BoldT -> ("\"", "\"")
    CodeT -> ("\"", "\"")

getJsonTag :: Either Data Object -> (String, String)
getJsonTag (Left data_) = getJsonDataTag data_
getJsonTag (Right obj) = getJsonObjectTag obj

printJsonEnd :: Maybe Handle -> Bool -> IO ()
printJsonEnd handle end
    | not end = printString handle ",\n" 0
    | otherwise = printString handle "\n" 0

printJsonData :: Maybe Handle -> Data -> Bool -> IO ()
printJsonData handle data_  end = do
    let (startTag, endTag) = getJsonDataTag data_
    printString handle "\""
    printString handle (fromJust (symbol data_))
    printString handle "\": "
    printString handle startTag
    printString handle (fromJust (dataContent data_))
    printString handle endTag
    printJsonEnd handle end

printJsonHeader :: Maybe Handle -> Header -> IO ()
printJsonHeader handle
    Header {title = title_, author = author_, date = date_} = do
    let (startTag, endTag) = getJsonObjectTag (Object SectionT Nothing [] [])
    printString handle startTag
    printJsonData handle (fromJust title_) False
    printJsonData handle (fromJust author_) False
    printJsonData handle (fromJust date_) True
    printString handle endTag

printJsonContent :: Maybe Handle -> Object -> IO ()
printJsonContent handle obj = printString handle "content"

printJson :: Maybe Handle -> DataStruct -> IO ()
printJson handle dataStruct = do
    printJsonHeader handle (header dataStruct)
    printJsonContent handle (content dataStruct)

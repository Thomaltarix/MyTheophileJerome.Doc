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
    SectionT -> ("{", "}")
    ListT -> ("[", "]")
    CodeBlockT -> ("[", "]")
    LinkT -> ("{", "}")
    ParagraphT -> ("[", "]")
    ImageT -> ("{", "}")

getJsonDataTag :: Data -> (String, String)
getJsonDataTag data_ = case dataType data_ of
    TextT -> ("\"", "\"")
    ItalicT -> ("\"", "\"")
    BoldT -> ("\"", "\"")
    CodeT -> ("\"", "\"")

printJsonEnd :: Maybe Handle -> Bool -> IO ()
printJsonEnd handle end
    | not end = printString handle ","
    | otherwise = return ()

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

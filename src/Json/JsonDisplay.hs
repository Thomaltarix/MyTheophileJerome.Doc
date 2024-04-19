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
printJsonData :: Maybe Handle -> Data -> Bool -> Int -> IO ()
printJsonData handle data_ end spaces =
    let (startTag, endTag) = getJsonDataTag data_ in
    printJsonSymbol handle (myFromJustString (symbol data_)) spaces >>
    printString handle startTag 0 >>
    printString handle (myFromJustString (dataContent data_)) 0 >>
    printString handle endTag 0 >>
    printJsonEnd handle end >>
    return ()

printContent :: Maybe Handle -> [Either Data Object] -> Int -> IO ()
printContent _ [] _ = return ()
printContent handle [x] spaces = case x of
    Left data_ -> printJsonData handle data_ True spaces
    Right obj -> printJsonObject handle obj True spaces
    >> printContent handle [] spaces
printContent handle (x:xs) spaces = case x of
    Left data_ -> printJsonData handle data_ False spaces
    Right obj -> printJsonObject handle obj False spaces
    >> printContent handle xs spaces

printJsonObject :: Maybe Handle -> Object -> Bool -> Int -> IO ()
printJsonObject handle obj end spaces =
    let (startTag, endTag) = getJsonObjectTag obj in
    printJsonSymbol handle (myFromJustString (objSymbol obj)) spaces >>
    printString handle startTag 0 >>
    printContent handle (datas obj) (spaces + 4) >>
    printString handle endTag spaces >>
    printJsonEnd handle end >>
    return ()

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

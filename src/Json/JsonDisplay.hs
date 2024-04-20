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

printJsonSymbol :: Maybe Handle -> String -> Int -> IO ()
printJsonSymbol handle "" spaces = printString handle "" spaces
printJsonSymbol handle symbol_ spaces =
    printString handle "\"" spaces >>
    printString handle symbol_ 0 >>
    printString handle "\": " 0

printJsonData :: Maybe Handle -> Data -> Bool -> Int -> IO ()
printJsonData handle data_ end spaces =
    let (startTag, endTag) = getJsonDataTag data_ in
    printJsonSymbol handle (myFromJustString (symbol data_)) spaces >>
    printString handle startTag 0 >>
    printString handle (myFromJustString (dataContent data_)) 0 >>
    printString handle endTag 0 >>
    printEnd handle end

printJsonContent :: Maybe Handle -> [Either Data Object] -> Int -> IO ()
printJsonContent _ [] _ = return ()
printJsonContent handle [x] spaces = case x of
    Left data_ -> printJsonData handle data_ True spaces
    Right obj -> printJsonObject handle obj True spaces
    >> printJsonContent handle [] spaces
printJsonContent handle (x:xs) spaces = case x of
    Left data_ -> printJsonData handle data_ False spaces
    Right obj -> printJsonObject handle obj False spaces
    >> printJsonContent handle xs spaces

printJsonObject :: Maybe Handle -> Object -> Bool -> Int -> IO ()
printJsonObject handle obj end spaces =
    let (startTag, endTag) = getJsonObjectTag obj in
    printJsonSymbol handle (myFromJustString (objSymbol obj)) spaces >>
    printString handle startTag 0 >>
    printJsonContent handle (datas obj) (spaces + 4) >>
    printString handle endTag spaces >>
    printEnd handle end

printJsonHeaderData :: Maybe Handle -> Maybe Data -> Bool -> Int -> IO ()
printJsonHeaderData _ Nothing _ _ = return ()
printJsonHeaderData handle (Just data_) end spaces =
    printJsonData handle data_ end spaces

printJsonHeader :: Maybe Handle -> Header -> Bool -> Int -> IO ()
printJsonHeader handle
    Header {title = title_, author = author_, date = date_} end spaces =
    let (startTag, endTag) = getJsonObjectTag (Object SectionT Nothing []) in
    printString handle "\"header\": " spaces >>
    printString handle startTag 0 >>
    printJsonHeaderData handle title_ False (spaces + 4) >>
    printJsonHeaderData handle author_ False (spaces + 4) >>
    printJsonHeaderData handle date_ True (spaces + 4) >>
    printString handle endTag spaces >>
    printEnd handle end

printJson :: Maybe Handle -> DataStruct -> IO ()
printJson handle dataStruct =
    let (startTag, endTag) = getJsonObjectTag (Object SectionT Nothing []) in
    printString handle startTag 0 >>
    printJsonHeader handle (header dataStruct) False 4 >>
    printJsonObject handle (content dataStruct) True 4 >>
    printString handle endTag 0 >>
    printString handle "\n" 0

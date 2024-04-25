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

printJsonSymbol ::String -> Int -> String
printJsonSymbol "" spaces = getString "" spaces
printJsonSymbol symbol_ spaces =
    getString "\"" spaces ++
    getString symbol_ 0 ++
    getString "\": " 0

printJsonData :: Data -> Bool -> Int -> String
printJsonData data_ end spaces =
    let (startTag, endTag) = getJsonDataTag data_ in
    printJsonSymbol (myFromJustString (symbol data_)) spaces ++
    getString startTag 0 ++
    getString (myFromJustString (dataContent data_)) 0 ++
    getString endTag 0 ++
    getEnd end

printJsonContent :: [Either Data Object] -> Int -> String
printJsonContent [] _ = ""
printJsonContent [Left data_] spaces = printJsonData data_ True spaces
printJsonContent [Right obj] spaces = printJsonObject obj True spaces
printJsonContent (Left data_:xs) spaces = printJsonData data_ False spaces
    ++ printJsonContent xs spaces
printJsonContent (Right obj:xs) spaces = printJsonObject obj False spaces
    ++ printJsonContent xs spaces

printJsonObject :: Object -> Bool -> Int -> String
printJsonObject obj end spaces =
    let (startTag, endTag) = getJsonObjectTag obj in
    printJsonSymbol (myFromJustString (objSymbol obj)) spaces ++
    getString startTag 0 ++
    printJsonContent (datas obj) (spaces + 4) ++
    getString endTag spaces ++
    getEnd end

printJsonHeaderData :: Maybe Data -> Bool -> Int -> String
printJsonHeaderData Nothing _ _ = ""
printJsonHeaderData (Just data_) end spaces =
    printJsonData data_ end spaces

printJsonHeader :: Header -> Bool -> Int -> String
printJsonHeader
    Header {title = title_, author = author_, date = date_} end spaces =
    let (startTag, endTag) = getJsonObjectTag (Object SectionT Nothing []) in
    getString "\"header\": " spaces ++
    getString startTag 0 ++
    printJsonHeaderData title_ False (spaces + 4) ++
    printJsonHeaderData author_ False (spaces + 4) ++
    printJsonHeaderData date_ True (spaces + 4) ++
    endTag ++
    getEnd end

printJson :: Maybe Handle -> DataStruct -> IO ()
printJson handle dataStruct =
    let (startTag, endTag) = getJsonObjectTag (Object SectionT Nothing []) in
    let json = startTag ++ printJsonHeader (header dataStruct) False 4
            ++ printJsonObject (content dataStruct) True 4 ++ endTag ++ "\n" in
        printString handle json

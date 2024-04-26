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

getJsonSymbol ::String -> Int -> String
getJsonSymbol "" spaces = getString "" spaces
getJsonSymbol symbol_ spaces =
    getString "\"" spaces ++
    getString symbol_ 0 ++
    getString "\": " 0

printJsonData :: Data -> Bool -> Int -> String
printJsonData data_ end spaces =
    let (startTag, endTag) = getJsonDataTag data_ in
    getJsonSymbol (myFromJustString (symbol data_)) spaces ++
    getString startTag 0 ++
    getString (myFromJustString (dataContent data_)) 0 ++
    getString endTag 0 ++
    getEnd end

getJsonContent :: [Either Data Object] -> Int -> String
getJsonContent [] _ = ""
getJsonContent [Left data_] spaces = printJsonData data_ True spaces
getJsonContent [Right obj] spaces = getJsonObject obj True spaces
getJsonContent (Left data_:xs) spaces = printJsonData data_ False spaces
    ++ getJsonContent xs spaces
getJsonContent (Right obj:xs) spaces = getJsonObject obj False spaces
    ++ getJsonContent xs spaces

getJsonObject :: Object -> Bool -> Int -> String
getJsonObject obj end spaces =
    let (startTag, endTag) = getJsonObjectTag obj in
    getJsonSymbol (myFromJustString (objSymbol obj)) spaces ++
    getString startTag 0 ++
    getJsonContent (datas obj) (spaces + 4) ++
    getString endTag spaces ++
    getEnd end

getJsonHeaderData :: Maybe Data -> Bool -> Int -> String
getJsonHeaderData Nothing _ _ = ""
getJsonHeaderData (Just data_) end spaces =
    printJsonData data_ end spaces

getJsonHeaderContent :: Header -> String -> Int -> Bool -> String
getJsonHeaderContent header_ "title" spaces end =
    getJsonHeaderData (title header_) end spaces
getJsonHeaderContent header_ "author" spaces end =
    getJsonHeaderData (author header_) end spaces
getJsonHeaderContent header_ "date" spaces end =
    getJsonHeaderData (date header_) end spaces
getJsonHeaderContent _ _ _ _ = ""

parseJsonHeaderContent :: Header ->
    (Maybe String, Maybe String, Maybe String) -> Int -> String
parseJsonHeaderContent header_
    (Just "title", Nothing, Nothing) spaces =
    getJsonHeaderContent header_ "title" spaces True
parseJsonHeaderContent header_
    (Just "title", Just "author", Nothing) spaces =
    getJsonHeaderContent header_ "title" spaces False
    ++ getJsonHeaderContent header_ "author" spaces True
parseJsonHeaderContent header_
    (Just "title", Just "date", Nothing) spaces =
    getJsonHeaderContent header_ "title" spaces False
    ++ getJsonHeaderContent header_ "date" spaces True
parseJsonHeaderContent header_
    (Just "title", Just "author", Just "date") spaces =
    getJsonHeaderContent header_ "title" spaces False
    ++ getJsonHeaderContent header_ "author" spaces False
    ++ getJsonHeaderContent header_ "date" spaces True
parseJsonHeaderContent header_
    (Just "title", Just "date", Just "author") spaces =
    getJsonHeaderContent header_ "title" spaces False
    ++ getJsonHeaderContent header_ "date" spaces False
    ++ getJsonHeaderContent header_ "author" spaces True
parseJsonHeaderContent _ _ _ = "test"

getJsonHeader :: Header -> Bool -> Int -> String
getJsonHeader
    header_@Header {order = order_} end spaces =
    let (startTag, endTag) = getJsonObjectTag (Object SectionT Nothing []) in
    getString "\"header\": " spaces ++
    getString startTag 0 ++
    parseJsonHeaderContent header_ order_ (spaces + 4) ++
    getString endTag spaces ++
    getEnd end

printJson :: Maybe Handle -> DataStruct -> IO ()
printJson handle dataStruct =
    let (startTag, endTag) = getJsonObjectTag (Object SectionT Nothing []) in
    let json = startTag ++ getJsonHeader (header dataStruct) False 4
            ++ getJsonObject (content dataStruct) True 4 ++ endTag ++ "\n" in
        printString handle json

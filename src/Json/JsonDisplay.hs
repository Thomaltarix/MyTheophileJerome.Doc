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

printData :: Maybe Handle -> Data -> Bool -> IO ()
printData handle data_  end = do
    let (startTag, endTag) = getJsonDataTag data_
    printString handle "\""
    printString handle (fromJust (symbol data_))
    printString handle "\": "
    printString handle startTag
    printString handle (fromJust (dataContent data_))
    printString handle endTag
    printEnd handle end

printJson :: Maybe Handle -> DataStruct -> IO ()
printJson _ _ = putStrLn "JSON"

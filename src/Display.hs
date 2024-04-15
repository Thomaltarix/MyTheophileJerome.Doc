{-
-- EPITECH PROJECT, 2024
-- MyTh-ophileJ-r-me.Doc
-- File description:
-- Display
-}

module Display (
    displayUsage,
    handleOutput,
    printString
    ) where

import DataStructure
import Json.JsonDisplay
import Xml.XmlDisplay
import Markdown.MarkdownDisplay

--
import Data.Maybe

-- Import for the open/close functions
import GHC.IO.IOMode
import GHC.IO.Handle.Text
import GHC.IO.Handle
import System.IO (openFile)

displayUsage :: IO ()
displayUsage = putStr  ("USAGE: ./mypandoc -i ifile -f oformat [-o ofile] " ++
                        "[-e iformat]\n" ++
                        "\tifilepath\tto the file to convert\n" ++
                        "\toformat\t\toutput format (xml, json, markdown)\n" ++
                        "\tofile\t\tpath to the output file\n" ++
                        "\tiformat\t\tinput format (xml, json, markdown)\n")

printString :: Maybe Handle -> String -> IO ()
printString Nothing str = putStrLn str
printString (Just handle) str = hPutStrLn handle str

parseOutputFormat :: String -> Maybe Handle -> DataStruct -> IO ()
parseOutputFormat "xml" handle dataStruct = printXml handle dataStruct
parseOutputFormat "json" handle dataStruct = printJson handle dataStruct
parseOutputFormat "markdown" handle dataStruct = printMarkdown handle dataStruct
parseOutputFormat _ handle _ = printString handle "Invalid output format"

handleOutput :: Conf -> DataStruct -> IO ()
handleOutput conf dataStruct
    | isNothing (outputFile conf) = parseOutputFormat (fromJust (outputFormat conf)) Nothing dataStruct
    | otherwise = do
        handle <- openFile (fromJust (outputFile conf)) WriteMode
        parseOutputFormat (fromJust (outputFormat conf)) (Just handle) dataStruct
        hClose handle

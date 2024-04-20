{-
-- EPITECH PROJECT, 2023
-- MyPandoc
-- File description:
-- MyPandoc
-}

module MyPandoc (
    myPandoc
    ) where

import System.Exit(exitWith, ExitCode(ExitFailure))
import ParsingCommand
import Display
import DataStructure (DataStruct)
import Json.ParseJson
import Control.Exception
import System.IO

myPandoc :: [String] -> IO ()
myPandoc [] = displayUsage >> exitWith(ExitFailure 84)
myPandoc args = case parseArgs args of
                        Nothing -> displayUsage >> exitWith(ExitFailure 84)
                        Just conf -> runPandoc conf

runPandoc :: Conf -> IO ()
runPandoc conf = do
    str <- myReadFile (getInputFile conf)
    case getStruct (inputFormat conf) str of
        (Just d) -> print d  -- launch display here
        Nothing ->  displayUsage >> exitWith(ExitFailure 84)

myReadFile :: String -> IO String
myReadFile input = do
    fileHandle <- try $
        openFile input ReadMode :: IO (Either SomeException Handle)
    case fileHandle of
        Right file -> hGetContents file
        Left _ -> displayUsage >> exitWith(ExitFailure 84)

getStruct :: Maybe String -> String -> Maybe DataStruct
getStruct (Just "json") str = jsonParsing str
getStruct (Just "xml") _ = Nothing
getStruct (Just "markdown") _ = Nothing
getStruct _ _ = Nothing

{-
-- EPITECH PROJECT, 2023
-- MyPandoc
-- File description:
-- ParsingCommand
-}

module ParsingCommand (
    Conf(..),
    parseArgs,
    getInputFile
    ) where

import DataStructure (Conf(..))
import System.Console.GetOpt
import Data.Maybe (isNothing)

defaultConf :: Conf
defaultConf = Conf
  { inputFile = Nothing,
    inputFormat = Nothing,
    outputFile = Nothing,
    outputFormat = Nothing
  }
getInputFile :: Conf -> String
getInputFile Conf {inputFile = Just i} = i
getInputFile _ = ""

options :: [OptDescr (Conf -> Conf)]
options =
  [ Option ['i'] ["inputfile"] (ReqArg (\arg opts -> opts
    { inputFile = Just arg }) "FILE") "path to input file"
  , Option ['f'] ["outputformat"] (ReqArg (\arg opts -> opts
    { outputFormat = Just arg }) "FILE") "format of the output"
  , Option ['o'] ["outputFile"] (ReqArg (\arg opts -> opts
    { outputFile = Just arg }) "FILE") "path to the output file"
  , Option ['e'] ["inputFormat"] (ReqArg (\arg opts -> opts
    { inputFormat = Just arg }) "FILE") "format of the input file"
  ]

checkValidityOptions :: Conf -> Bool
checkValidityOptions opts = isNothing (inputFile opts) ||
                            isNothing (outputFormat opts)

getConfByOptions :: [Conf -> Conf] -> Maybe Conf
getConfByOptions opts = let conf = foldl (flip id) defaultConf opts
                        in if not (checkValidityOptions conf) then Just conf
                        else Nothing

getOptions :: [Conf -> Conf] -> [String] -> [String] -> Maybe Conf
getOptions opts [] [] = getConfByOptions opts
getOptions _ _ _ = Nothing

parseArgs :: [String] -> Maybe Conf
parseArgs args =
    let (opts, nopt, errs) = getOpt Permute options args
    in getOptions opts errs nopt

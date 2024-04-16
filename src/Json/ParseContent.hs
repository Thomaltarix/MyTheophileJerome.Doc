{-
-- EPITECH PROJECT, 2023
-- MyPandoc
-- File description:
-- ParseContent
-}

module Json.ParseContent (
    getContent,
    ) where

-- import ParsingLib
import DataStructure

getContent :: String -> Object -> (Maybe Object, String)
getContent _ o = (Just o, "")
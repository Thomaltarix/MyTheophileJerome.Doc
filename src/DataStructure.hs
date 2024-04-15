{-
-- EPITECH PROJECT, 2023
-- MyPandoc
-- File description:
-- DataStructure
-}

module DataStructure (
    DataStruct(..),
    Header(..),
    Object(..),
    Data(..),
    ObjectType(..),
    Conf(..)
    ) where


data Conf = Conf
    {
        inputFile :: Maybe String,
        inputFormat :: Maybe String,
        outputFile :: Maybe String,
        outputFormat :: Maybe String
    } deriving Show

-- main structure
data DataStruct = DataStruct
    {
        header :: Header,
        content :: Object
    } deriving Eq

-- enum
data ObjectType = ObjectT | ListT deriving Eq

-- Header
data Header = Header
    {
        title :: Maybe Data,
        author :: Maybe Data,
        date :: Maybe Data
    } deriving Eq

-- Objects
data Object = Object
    {
        dataType :: ObjectType,
        datas :: [Data],
        objects :: [Object],
        objSymbol :: Maybe String
    } deriving Eq

data Data = Data
    {
        dataContent :: Maybe String,
        symbol :: Maybe String
    } deriving Eq

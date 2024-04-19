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
    Conf(..),
    DataType(..)
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

-- Header
data Header = Header
    {
        title :: Maybe Data,
        author :: Maybe Data,
        date :: Maybe Data
    } deriving Eq

-- enum
data ObjectType = SectionT | ListT | CodeBlockT | LinkT | ParagraphT |ImageT | AltT  deriving Eq

data DataType = TextT | ItalicT | BoldT | CodeT deriving Eq

-- Objects
data Object = Object
    {
        objType :: ObjectType,
        objSymbol :: Maybe String,
        datas :: [Either Data Object]
    } deriving Eq

data Data = Data
    {
        dataContent :: Maybe String,
        dataType :: DataType,
        symbol :: Maybe String
    } deriving Eq

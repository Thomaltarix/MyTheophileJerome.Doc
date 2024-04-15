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
    } deriving (Eq, Show)

-- Header
data Header = Header
    {
        title :: Maybe Data,
        author :: Maybe Data,
        date :: Maybe Data
    } deriving (Eq, Show)

-- enum
data ObjectType = SectionT | ListT | CodeBlockT  deriving (Eq, Show)

data DataType = TextT | ItalicT | BoldT | CodeT | LinkT | ImageT | ParagraphT deriving (Eq, Show)

-- Objects
data Object = Object
    {
        objType :: ObjectType,
        objSymbol :: Maybe String,
        datas :: [Data],
        objects :: [Object]
    } deriving (Eq, Show)

data Data = Data
    {
        dataContent :: Maybe String,
        dataType :: DataType,
        symbol :: Maybe String
    } deriving (Eq, Show)

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
    DataType(..),
    Conf(..),
    defaultHeader,
    createData,
    createObject,
    defaultObject
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

defaultHeader :: Header
defaultHeader = Header 
    {
    title = Nothing,
    author = Nothing,
    date = Nothing
    }

defaultObject :: Object
defaultObject = Object 
    {
    objType = SectionT,
    objSymbol = Nothing,
    datas = [],
    objects = []
    }

createData :: String -> DataType -> String -> Data
createData c t s = Data{dataContent = Just c, dataType = t, symbol = Just s}

createObject :: ObjectType -> String -> [Data] -> [Object] -> Object
createObject t s d o = Object {
    objType = t,
    objSymbol  = Just s,
    datas = d,
    objects = o
    }
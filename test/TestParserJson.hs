{-
-- EPITECH PROJECT, 2024
-- MyTh-ophileJ-r-me.Doc
-- File description:
-- Spec Json Parser
-}

module TestParserJson (
    jsonTestsParsing
) where

import Test.Hspec

import Json.ParseHeader
import Json.ParseContent
import DataStructure
import ParsingLib

jsonTestsParsing :: IO ()
jsonTestsParsing = jsonHeaderTests >> jsonContentTests

jsonHeaderTests :: IO ()
jsonHeaderTests = hspec $ do
    it "getHeader whithout Author And Date" $ do getHeader "{ \"header\": {\"title\": \"Syntaxe JSON\"}" defaultHeader
        `shouldBe` ( Just Header
        {
            title = Just ( Data { dataContent = Just "Syntaxe JSON", dataType = TextT, symbol = Just "title"}),
            author = Nothing,
            date = Nothing
        }, "")

    it "getHeader whithout Date" $ do getHeader "{ \"header\": {\"title\": \"Syntaxe JSON\", \"author\": \"Hello\"}" defaultHeader
        `shouldBe` ( Just Header
        {
            title = Just ( Data { dataContent = Just "Syntaxe JSON", dataType = TextT, symbol = Just "title"}),
            author = Just ( Data { dataContent = Just "Hello", dataType = TextT, symbol = Just "author"}),
            date = Nothing
        }, "")

    it "getHeader whithout Author" $ do getHeader "{ \"header\": {\"title\": \"Syntaxe JSON\", \"date\": \"2101\"}" defaultHeader
        `shouldBe` ( Just Header
        {
            title = Just ( Data { dataContent = Just "Syntaxe JSON", dataType = TextT, symbol = Just "title"}),
            date = Just ( Data { dataContent = Just "2101", dataType = TextT, symbol = Just "date"}),
            author = Nothing
        }, "")

    it "getHeader whithout Title" $ do getHeader "{\"author\": \"Hello\"}" defaultHeader
        `shouldBe` (Nothing, "{\"author\": \"Hello\"}")

jsonContentTests :: IO ()
jsonContentTests = hspec $ do
    it "should return Nothing when given an empty string" $
      getContent "" `shouldBe` (Nothing, "")

    it "body false" $
        runParser parseJsonBody "" `shouldBe` Nothing
    it "body true" $
        runParser parseJsonBody "\"body\": [ ] }" `shouldBe` Just (Object {objType = ListT, objSymbol = Just "body", datas = []},"")

    it "Array false" $
        runParser parseJsonArray "" `shouldBe` Nothing
    it "Array true" $
        runParser parseJsonArray "[ \"ok\" ]" `shouldBe` Just (Right (Object {objType = ListT, objSymbol = Nothing, datas = [Left (Data {dataContent = Just "ok", dataType = TextT, symbol = Nothing})]}),"")

    it "Object false" $
        runParser parseJsonObject "" `shouldBe` Nothing
    it "Object true" $
        runParser parseJsonObject "{ \"section\": {} }" `shouldBe` Just (Right (Object {objType = SectionT, objSymbol = Nothing, datas = [Right (Object {objType = SectionT, objSymbol = Just "section", datas = []})]}),"")

    it "Section false" $
        runParser parseJsonSection "" `shouldBe` Nothing
    it "Section true" $
        runParser parseJsonSection "\"section\": { }" `shouldBe` Just (Right (Object {objType = SectionT, objSymbol = Just "section", datas = []}), "")

    it "Title false" $
        runParser parseJsonTitle "" `shouldBe` Nothing
    it "Title true" $
        runParser parseJsonTitle "\"title\": \"ok\"" `shouldBe` Just (Left (Data {dataContent = Just "ok", dataType = TextT, symbol = Just "title"}),"")

    it "Content false" $
        runParser parseJsonContent "" `shouldBe` Nothing
    it "Content true" $
        runParser parseJsonContent "\"content\": []" `shouldBe` Just (Right Object {objType = ListT,objSymbol = Just "content",datas = []}, "")

    it "Bold false" $
        runParser parseJsonBold "" `shouldBe` Nothing
    it "Bold true" $
        runParser parseJsonBold "\"bold\": \"ok\"" `shouldBe` Just (Left Data {
                   dataContent = Just "ok",
                   dataType = BoldT,
                   symbol = Just "bold"
                 }, "")

    it "Italic false" $
        runParser parseJsonItalic "" `shouldBe` Nothing
    it "Italic true" $
        runParser parseJsonItalic "\"italic\": \"ok\"" `shouldBe` Just (Left Data {
                   dataContent = Just "ok",
                   dataType = ItalicT,
                   symbol = Just "italic"
                 }, "")

    it "Code false" $
        runParser parseJsonCode "" `shouldBe` Nothing
    it "Code true" $
        runParser parseJsonCode "\"code\": \"ok\"" `shouldBe` Just (Left Data {
                   dataContent = Just "ok",
                   dataType = CodeT,
                   symbol = Just "code"
                 }, "")

    it "CodeBlock false" $
        runParser parseJsonCodeBlock "" `shouldBe` Nothing
    it "CodeBlock true" $
        runParser parseJsonCodeBlock "\"codeblock\": [\"ok\"]" `shouldBe` Just (Right Object {
                   objType = CodeBlockT,
                   objSymbol = Just "codeblock",
                   datas = [Left Data {
                   dataContent = Just "ok",
                   dataType = TextT,
                   symbol = Nothing
                 }]
                 }, "")


    it "List false" $
        runParser parseJsonList "" `shouldBe` Nothing
    it "List true" $
        runParser parseJsonList "\"list\": []" `shouldBe` Just (Right Object {
                   objType = ListT,
                   objSymbol = Just "list",
                   datas = []
                 }, "")


    it "Url false" $
        runParser parseJsonUrl "" `shouldBe` Nothing
    it "Url true" $
        runParser parseJsonUrl "\"url\": \"url\"" `shouldBe` Just (Left Data {
                   dataContent = Just "url",
                   dataType = TextT,
                   symbol = Just "url"
                 }, "")


    it "Image false" $
        runParser parseJsonImage "" `shouldBe` Nothing
    it "Image true" $
        runParser parseJsonImage "\"image\": {}" `shouldBe` Just (Right Object {
                   objType = ImageT,
                   objSymbol = Just "image",
                   datas = []
                 }, "")


    it "Alt false" $
        runParser parseJsonAlt "" `shouldBe` Nothing
    it "Alt true" $
        runParser parseJsonAlt "\"alt\": []" `shouldBe` Just (Right Object {
                   objType = AltT,
                   objSymbol = Just "alt",
                   datas = []
                 }, "")


    it "Link false" $
        runParser parseJsonLink "" `shouldBe` Nothing
    it "Link true" $
        runParser parseJsonLink "\"link\": {}" `shouldBe` Just (Right Object {
                   objType = LinkT,
                   objSymbol = Just "link",
                   datas = []
                 }, "")


    it "StringQuote false" $
        runParser parseJsonStringQuote "" `shouldBe` Nothing
    it "StringQuote true" $
        runParser parseJsonStringQuote "\" Hello\"" `shouldBe` Just (Left (Data {dataContent = Just " Hello", dataType = TextT, symbol = Nothing}),"")

    it "Int false" $
        runParser parseJsonInt "" `shouldBe` Nothing
    it "Int true" $
        runParser parseJsonInt "12345" `shouldBe` Just (Left (Data {dataContent = Just "12345", dataType = TextT, symbol = Nothing}),"")  
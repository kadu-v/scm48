module ParserSpec (spec) where

import Parser
import Syntax
import Test.Hspec
import Text.ParserCombinators.Parsec hiding (spaces)

spec :: Spec
spec = do
  describe "test of parseString" $ do
    it "parseString \"foo\"" $ parse parseString "scm48" "\"foo\"" `shouldBe` (Right $ String "foo")
    it "parseString \"bar\"" $ parse parseString "scm48" "\"bar\"" `shouldBe` (Right $ String "bar")
    it "parseString \"xx1\"" $ parse parseString "scm48" "\"xx1\"" `shouldBe` (Right $ String "xx1")
    it "parseString \"xx\"" $ parse parseString "scm48" "\"\\\"xx\"" `shouldBe` (Right $ String "\"xx")
    it "parseString \"\\yy\"" $ parse parseString "scm48" "\"\\\\yy\"" `shouldBe` (Right $ String "\\yy")
    it "parseString \"\\zz\\n\"" $ parse parseString "scm48" "\"zz\\n\"" `shouldBe` (Right $ String "zz\n")
    it "parseString \"\\vv\\r\"" $ parse parseString "scm48" "\"vv\\r\"" `shouldBe` (Right $ String "vv\r")
    it "parseString \"\\ww\\t\"" $ parse parseString "scm48" "\"ww\\t\"" `shouldBe` (Right $ String "ww\t")
    it "parseString \"\\uu\\\\\"" $ parse parseString "scm48" "\"uu\\\\\"" `shouldBe` (Right $ String "uu\\")

  describe "test of parseAtom" $ do
    it "parseAtom x" $ parse parseAtom "scm48" "x" `shouldBe` (Right $ Atom "x")
    it "parseAtom var" $ parse parseAtom "scm48" "var" `shouldBe` (Right $ Atom "var")
    it "parseAtom var1" $ parse parseAtom "scm48" "var1" `shouldBe` (Right $ Atom "var1")
    it "parseAtom hoge_huga" $ parse parseAtom "scm48" "hoge_huga" `shouldBe` (Right $ Atom "hoge_huga")
    it "parseAtom xx-yyy" $ parse parseAtom "scm48" "xx-yy" `shouldBe` (Right $ Atom "xx-yy")

  describe "test of parseNumber" $ do
    it "parseNumber 0" $ parse parseNumber "scm48" "0" `shouldBe` (Right $ Number 0)
    it "parseNumber 123" $ parse parseNumber "scm48" "123" `shouldBe` (Right $ Number 123)
    it "parseNumber 1234" $ parse parseNumber "scm48" "1234" `shouldBe` (Right $ Number 1234)
    it "parseNumber 1111" $ parse parseNumber "scm48" "1111" `shouldBe` (Right $ Number 1111)

  describe "test of parseBool" $ do
    it "parseBool #t" $ parse parseBool "scm48" "#t" `shouldBe` (Right $ Bool True)
    it "parseBool #f" $ parse parseBool "scm48" "#f" `shouldBe` (Right $ Bool False)

  describe "test of parseList" $ do
    it "parseList \" \"" $ parse parseList "scm48" " " `shouldBe` (Right $ List [])
    it "parseList \"1\"" $ parse parseList "scm48" "1" `shouldBe` (Right $ List [Number 1])
    it "parseList \"1 #t \"hoge\"\"" $ parse parseList "scm48" "1 #t \"hoge\"" `shouldBe` (Right $ List [Number 1, Bool True, String "hoge"])

  describe "test of parseDottedList" $ do
    it "parseDottedList 1 . 2" $ parse parseDottedList "scm48" "1 . 2" `shouldBe` (Right $ DottedList [Number 1] (Number 2))
    it "parseDottedList 1 . \"hoge\"" $ parse parseDottedList "scm48" "1 . \"hoge\"" `shouldBe` (Right $ DottedList [Number 1] (String "hoge"))
    it "parseDottedList 1 . #t" $ parse parseDottedList "scm48" "1 . #t" `shouldBe` (Right $ DottedList [Number 1] (Bool True))

  describe "test of parseQuoted" $ do
    it "parseQuoted \'x" $ parse parseQuoted "scm48" "\'x" `shouldBe` (Right $ List [Atom "quote", Atom "x"])
    it "parseQuoted \'1" $ parse parseQuoted "scm48" "\'1" `shouldBe` (Right $ List [Atom "quote", Number 1])
    it "parseQuoted \'#t" $ parse parseQuoted "scm48" "\'#t" `shouldBe` (Right $ List [Atom "quote", Bool True])

  describe "test of parseExpr" $ do
    it "parseExpr xx-yy" $ parse parseExpr "scm48" "xx-yy" `shouldBe` (Right $ Atom "xx-yy")
    it "parseExpr 1234" $ parse parseExpr "scm48" "1234" `shouldBe` (Right $ Number 1234)
    it "parseExpr \"hoge\"" $ parse parseExpr "scm48" "\"hoge\"" `shouldBe` (Right $ String "hoge")
    it "parseExpr #t" $ parse parseExpr "scm48" "#t" `shouldBe` (Right $ Bool True)
    it "parseExpr #f" $ parse parseExpr "scm48" "#f" `shouldBe` (Right $ Bool False)
    it "parrseExpr ()" $ parse parseExpr "scm48" "()" `shouldBe` (Right $ List [])
    it "parrseExpr (1 2 3)" $ parse parseExpr "scm48" "(1 2 3)" `shouldBe` (Right $ List [Number 1, Number 2, Number 3])
    it "parrseExpr (1 . (#t . (\"hoge\" . (1 2 3))))" $
      parse parseExpr "scm48" "(1 . (#t . (\"hoge\" . (1 2 3))))"
        `shouldBe` (Right $ DottedList [Number 1] $ DottedList [Bool True] $ DottedList [String "hoge"] $ List [Number 1, Number 2, Number 3])
    it "parseExpr '(1 2 3)" $ parse parseExpr "scm48" "\'(1 2 3)" `shouldBe` (Right $ List [Atom "quote", List [Number 1, Number 2, Number 3]])

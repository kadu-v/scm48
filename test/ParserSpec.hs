module ParserSpec (spec) where

import Parser
import Syntax
import Test.Hspec
import Text.ParserCombinators.Parsec hiding (spaces)

spec :: Spec
spec = do
  describe "test of parseString" $ do
    it "parseString foo" $ parse parseString "scm48" "\"foo\"" `shouldBe` (Right $ String "foo")
    it "parseString bar" $ parse parseString "scm48" "\"bar\"" `shouldBe` (Right $ String "bar")
    it "parseString xx1" $ parse parseString "scm48" "\"xx1\"" `shouldBe` (Right $ String "xx1")

  describe "test of parseAtom" $ do
    it "parseAtom x" $ parse parseAtom "scm48" "x" `shouldBe` (Right $ Atom "x")
    it "parseAtom var" $ parse parseAtom "scm48" "var" `shouldBe` (Right $ Atom "var")
    it "parseAtom var1" $ parse parseAtom "scm48" "var1" `shouldBe` (Right $ Atom "var1")
    it "parseAtom hoge_huga" $ parse parseAtom "scm48" "hoge_huga" `shouldBe` (Right $ Atom "hoge_huga")
    it "parseAtom xx-yyy" $ parse parseAtom "scm48" "xx-yy" `shouldBe` (Right $ Atom "xx-yy")
    it "parseAtom #t" $ parse parseAtom "scm48" "#t" `shouldBe` (Right $ Bool True)
    it "parseAtom #f" $ parse parseAtom "scm48" "#f" `shouldBe` (Right $ Bool False)
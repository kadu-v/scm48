module EvalSpec (spec) where

import Eval
import Parser
import Syntax
import Test.Hspec

evalAndParse = eval . readExpr

spec :: Spec
spec = do
  describe "test of eval" $ do
    it "eval \"hoge\"" $ evalAndParse "\"hoge\"" `shouldBe` (String "hoge")
    it "eval 123" $ evalAndParse "123" `shouldBe` (Number 123)
    it "eval #t" $ evalAndParse "#t" `shouldBe` (Bool True)
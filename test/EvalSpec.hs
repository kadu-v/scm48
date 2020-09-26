module EvalSpec (spec) where

import Eval
import Parser
import Syntax
import Test.Hspec

evalAndParse = eval . readExpr

spec :: Spec
spec = do
  describe "simple test of eval" $ do
    it "eval \"hoge\"" $ evalAndParse "\"hoge\"" `shouldBe` (String "hoge")
    it "eval 123" $ evalAndParse "123" `shouldBe` (Number 123)
    it "eval #t" $ evalAndParse "#t" `shouldBe` (Bool True)

  describe "primitive operator test of eval" $ do
    it "operator: +" $ evalAndParse "(+ 1 1)" `shouldBe` (Number 2)
    it "operator: -" $ evalAndParse "(- 2 3)" `shouldBe` (Number (-1))
    it "operator: *" $ evalAndParse "(* 1 2)" `shouldBe` (Number 2)
    it "operator: /" $ evalAndParse "(/ 1 1)" `shouldBe` (Number 1)
    it "operator: mod" $ evalAndParse "(mod 5 3)" `shouldBe` (Number 2)
    it "operator: quotient" $ evalAndParse "(quotient 5 3)" `shouldBe` (Number 1)
    it "operator: remainder" $ evalAndParse "(remainder 3 7)" `shouldBe` (Number 3)

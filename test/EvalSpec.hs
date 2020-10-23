module EvalSpec (spec) where

import Eval
import Parser
import Syntax
import Test.Hspec

evalAndParse :: String -> LispVal
evalAndParse input =
  let val = do
        x <- readExpr input
        y <- eval x
        return y
   in case val of
        Right x -> x
        Left err -> String $ "error: " ++ show err

spec :: Spec
spec = do
  describe "simple test of eval" $ do
    it "eval \"hoge\"" $ evalAndParse "\"hoge\"" `shouldBe` (String "hoge")
    it "eval 123" $ evalAndParse "123" `shouldBe` (Number 123)
    it "eval #t" $ evalAndParse "#t" `shouldBe` (Bool True)

  describe "primitive operator test of eval" $ do
    it "operator: (+ 1 1)" $ evalAndParse "(+ 1 1)" `shouldBe` (Number 2)
    it "operator: (- 2 3)" $ evalAndParse "(- 2 3)" `shouldBe` (Number (-1))
    it "operator: (* 1 2)" $ evalAndParse "(* 1 2)" `shouldBe` (Number 2)
    it "operator: (/ 1 2)" $ evalAndParse "(/ 1 1)" `shouldBe` (Number 1)
    it "operator: (mod 5 3)" $ evalAndParse "(mod 5 3)" `shouldBe` (Number 2)
    it "operator: (quotient 5 3)" $ evalAndParse "(quotient 5 3)" `shouldBe` (Number 1)
    it "operator: (remainder 3 7)" $ evalAndParse "(remainder 3 7)" `shouldBe` (Number 3)

    it "operator: (= 1 1)" $ evalAndParse "(= 1 1)" `shouldBe` (Bool True)
    it "oeprator: (= 1 2)" $ evalAndParse "(= 1 2)" `shouldBe` (Bool False)

    it "oeprator: (> 1 2)" $ evalAndParse "(> 2 1)" `shouldBe` (Bool True)
    it "operator: (> 1 1)" $ evalAndParse "(> 1 1)" `shouldBe` (Bool False)
    it "operator: (> 2 1)" $ evalAndParse "(> 1 2)" `shouldBe` (Bool False)

    it "oeprator: (< 1 2)" $ evalAndParse "(= 1 2)" `shouldBe` (Bool False)
    it "oeprator: (< 1 2)" $ evalAndParse "(> 2 1)" `shouldBe` (Bool True)
    it "operator: (< 1 1)" $ evalAndParse "(> 1 1)" `shouldBe` (Bool False)

    it "operator: (/= 1 2)" $ evalAndParse "(/= 1 2)" `shouldBe` (Bool True)
    it "operator: (/= 1 1)" $ evalAndParse "(/= 1 1)" `shouldBe` (Bool False)
    it "operator: (/= 3 2)" $ evalAndParse "(/= 3 2)" `shouldBe` (Bool True)

    it "operator: (>= 1 2)" $ evalAndParse "(>= 1 2)" `shouldBe` (Bool False)
    it "operator: (>= 1 1)" $ evalAndParse "(>= 1 1)" `shouldBe` (Bool True)
    it "operator: (>= 3 2)" $ evalAndParse "(>= 3 2)" `shouldBe` (Bool True)

    it "oeprator: (<= 3 2)" $ evalAndParse "(<= 3 2)" `shouldBe` (Bool False)
    it "oeprator: (<= 1 2)" $ evalAndParse "(<= 1 2)" `shouldBe` (Bool True)
    it "operator: (<= 1 1)" $ evalAndParse "(<= 1 1)" `shouldBe` (Bool True)

    it "oeprator: (&& #f #f)" $ evalAndParse "(&& #f #f)" `shouldBe` (Bool False)
    it "operator: (&& #t #f)" $ evalAndParse "(&& #t #f)" `shouldBe` (Bool False)
    it "operator: (&& #t #t)" $ evalAndParse "(&& #t #t)" `shouldBe` (Bool True)

    it "oeprator: (|| #f #f)" $ evalAndParse "(|| #f #f)" `shouldBe` (Bool False)
    it "oeprator: (|| #t #f)" $ evalAndParse "(|| #t #f)" `shouldBe` (Bool True)
    it "oeprator: (|| #t #t)" $ evalAndParse "(|| #t #t)" `shouldBe` (Bool True)

    it "oeprator: (string=? \"hoge\" \"hoge\")" $ evalAndParse "(string=? \"hoge\" \"hoge\")" `shouldBe` (Bool True)
    it "operator: (string=? \"hoge\" \"foo\")" $ evalAndParse "(string=? \"hoge\" \"foo\")" `shouldBe` (Bool False)

    it "oeprator: (string>? \"foo\" \"hoge\")" $ evalAndParse "(string>? \"foo\" \"hoge\")" `shouldBe` (Bool False)
    it "oeprator: (string>? \"hoge\" \"hoge\")" $ evalAndParse "(string>? \"hoge\" \"hoge\")" `shouldBe` (Bool False)
    it "oeprator: (string>? \"hoge\" \"bar\")" $ evalAndParse "(string>? \"hoge\" \"bar\")" `shouldBe` (Bool True)

    it "oeprator: (string<? \"foo\" \"hoge\")" $ evalAndParse "(string<? \"foo\" \"hoge\")" `shouldBe` (Bool True)
    it "oeprator: (string<? \"hoge\" \"hoge\")" $ evalAndParse "(string<? \"hoge\" \"hoge\")" `shouldBe` (Bool False)
    it "oeprator: (string<? \"hoge\" \"bar\")" $ evalAndParse "(string<? \"hoge\" \"bar\")" `shouldBe` (Bool False)

    it "oeprator: (string>=? \"foo\" \"hoge\")" $ evalAndParse "(string>=? \"foo\" \"hoge\")" `shouldBe` (Bool False)
    it "oeprator: (string>=? \"hoge\" \"hoge\")" $ evalAndParse "(string>=? \"hoge\" \"hoge\")" `shouldBe` (Bool True)
    it "oeprator: (string>=? \"hoge\" \"bar\")" $ evalAndParse "(string>=? \"hoge\" \"bar\")" `shouldBe` (Bool True)

    it "oeprator: (string<=? \"foo\" \"hoge\")" $ evalAndParse "(string<=? \"foo\" \"hoge\")" `shouldBe` (Bool True)
    it "oeprator: (string<=? \"hoge\" \"hoge\")" $ evalAndParse "(string<=? \"hoge\" \"hoge\")" `shouldBe` (Bool True)
    it "oeprator: (string<=? \"hoge\" \"bar\")" $ evalAndParse "(string<=? \"hoge\" \"bar\")" `shouldBe` (Bool False)

    it "operator: (car '(1 2))" $ evalAndParse "(car '(1 2))" `shouldBe` (Number 1)
    it "operator: (car '(1 2 3))" $ evalAndParse "(car '(1 2 3))" `shouldBe` (Number 1)
    it "operator: (car '(#t #f #t)" $ evalAndParse "(car '(#t #f #t))" `shouldBe` (Bool True)

    it "operator: (cdr '(1 2))" $ evalAndParse "(cdr '(1 2))" `shouldBe` (List [Number 2])
    it "operator: (cdr '(1 2 3))" $ evalAndParse "(cdr '(1 2 3))" `shouldBe` (List [Number 2, Number 3])
    it "operator: (cdr '(#t #f #t))" $ evalAndParse "(cdr '(#t #f #t))" `shouldBe` (List [Bool False, Bool True])

    it "operator: (cons 1 '())" $ evalAndParse "(cons 1 '())" `shouldBe` (List [Number 1])
    it "operatir: (cons 1 '(2 3))" $ evalAndParse "(cons 1 '(2 3))" `shouldBe` (List [Number 1, Number 2, Number 3])
    it "operator: (cons #t '(#f #t))" $ evalAndParse "(cons #t '(#f #t))" `shouldBe` (List [Bool True, Bool False, Bool True])

    it "operator: (eqv #t #t)" $ evalAndParse "(eqv #t #t)" `shouldBe` (Bool True)
    it "oeprator: (eqv #t #f)" $ evalAndParse "(eqv #t #f)" `shouldBe` (Bool False)
    it "operator: (eqv 1 1)" $ evalAndParse "(eqv 1 1)" `shouldBe` (Bool True)
    it "operator: (eqv 1 2)" $ evalAndParse "(eqv 1 2)" `shouldBe` (Bool False)
    it "operator: (eqv \"hoge\" \"hoge\")" $ evalAndParse "(eqv \"hoge\" \"hoge\")" `shouldBe` (Bool True)
    it "operator: (eqv \"hoge\" \"foo\")" $ evalAndParse "(eqv \"hoge\" \"foo\")" `shouldBe` (Bool False)
    it "operator: (eqv '(1 2 3) '(1 2 3)" $ evalAndParse "(eqv '(1 2 3) '(1 2 3))" `shouldBe` (Bool True)
    it "oparator: (eqv '(1 2 3) '(1 2 4)" $ evalAndParse "(eqv '(1 2 3) '(1 2 4))" `shouldBe` (Bool False)

    it "operator: (equal 1 1)" $ evalAndParse "(equal 1 1)" `shouldBe` (Bool True)
    it "operator: (equal 1 2)" $ evalAndParse "(equal 1 2)" `shouldBe` (Bool False)
    it "operator: (equal 1 \"1\")" $ evalAndParse "(equal 1 \"1\")" `shouldBe` (Bool True)
    it "operator: (equal #t #t)" $ evalAndParse "(equal #t #t)" `shouldBe` (Bool True)
    it "operaotr: (equal #t #f)" $ evalAndParse "(equal #t #f)" `shouldBe` (Bool False)
    it "operator: (equal #t \"#t\")" $ evalAndParse "(equal #t \"#t\")" `shouldBe` (Bool False)
    it "operator: (equal \"hoge\" \"hoge\")" $ evalAndParse "(equal \"hoge\" \"hoge\")" `shouldBe` (Bool True)

  describe "if expression test of eval" $ do
    it "(if #t 1 2)" $ evalAndParse "(if #t 1 2)" `shouldBe` (Number 1)
    it "(if #f 1 2)" $ evalAndParse "(if #f 1 2)" `shouldBe` (Number 2)
    it "(if (= 1 1) \"yes\" \"no\")" $ evalAndParse "(if (= 1 1) \"yes\" \"no\")" `shouldBe` (String "yes")
    it "(if (> 1 2) #t #f)" $ evalAndParse "(if (> 1 2) #t #f)" `shouldBe` (Bool False)

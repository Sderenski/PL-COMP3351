module EvalSpec where


    import Test.Hspec
    import Parser
    import Expr
    import MiniRacketParser

    import Eval
    import Error

    type ParseResult = Either ErrorType (Expr, String)

    spec :: Spec
    spec = do
        describe "eval expressions" $ do
            it "evaluates number: 1235" $ 
                evalString "1235" `shouldBe` Right (IntValue 1235)
            it "evaluates negative numbers: -12235" $
                evalString "-12235" `shouldBe` Right (IntValue (-12235))
            it "evaluates true" $
                evalString "true" `shouldBe` Right (BoolValue True)
            it "evaluates false" $
                evalString "false" `shouldBe` Right (BoolValue False)
        
        describe "eval Bool expressions" $ do
            it "evaluates (not false)" $
                evalString "(not false)" `shouldBe` Right (BoolValue True)
            it "evaluates (not true)" $
                evalString "(not true)" `shouldBe` Right (BoolValue False)
            it "evaluates (and true false)" $
                evalString "(and true false)" `shouldBe` Right (BoolValue False)
            it "evaluates (and false (and true false) true)" $
                evalString "(and false (and true false) true)" `shouldBe` Right (BoolValue False)
            it "evaluates (and true (or false true) (not false))" $
                evalString "(and true (or false true) (not false))" `shouldBe` Right (BoolValue True)
            it "evalutes (or true false)" $
                evalString "(or true false)" `shouldBe` Right (BoolValue True)

        describe "eval Math expressions" $ do
            it "evaluates (+ 5 5)" $
                evalString "(+ 5 5)" `shouldBe` Right (IntValue 10)
            it "evaluates (+ 2 5 2)" $
                evalString "(+ 2 5 2)" `shouldBe` Right (IntValue 9)
            it "evaluates (+ 8 2)" $
                evalString "(+ 8 2)" `shouldBe` Right (IntValue 10)
            
            it "evaluates (- 5 1)" $
                evalString "(- 5 1)" `shouldBe` Right (IntValue 4)
            it "evaluates (- 5 1 1)" $
                evalString "(- 5 1 1)" `shouldBe` Right (IntValue 3)
            it "evaluates (- 2 4)" $
                evalString "(- 2 4)" `shouldBe` Right (IntValue (-2))

            it "evaluates (* 5 2)" $
                evalString "(* 5 2)" `shouldBe` Right (IntValue 10)
            it "evaluates (* 5 2 2)" $
                evalString "(* 5 2 2)" `shouldBe` Right (IntValue 20)
            it "evaluates (* 5 -2)" $
                evalString "(* 5 -2)" `shouldBe` Right (IntValue (-10)) 
            
            it "evaluates (div 10 2)" $
                evalString "(div 10 2)" `shouldBe` Right (IntValue 5)
            it "evaluates (div 2 2)" $
                evalString "(div 2 2)" `shouldBe` Right (IntValue 1)
            it "evaluates (div -8 4)" $
                evalString "(div -8 4)" `shouldBe` Right (IntValue (-2))

            it "evaluates (mod 10 2)" $
                evalString "(mod 10 2)" `shouldBe` Right (IntValue 0)
            it "evaluates (mod 11 2)" $
                evalString "(mod 11 2)" `shouldBe` Right (IntValue 1)
            it "evaluates (mod 8 3)" $
                evalString "(mod 8 3)" `shouldBe` Right (IntValue 2)

        describe "eval Comparasion Expression" $ do
            it "evaluates (< 5 6)" $
                evalString "(< 5 6)" `shouldBe` Right (BoolValue True)
            it "evaluates (< 10 2)" $
                evalString "(< 10 2)" `shouldBe` Right (BoolValue False)
            it "evaluates (< 1 8)" $
                evalString "(< 1 8)" `shouldBe` Right (BoolValue True)

            it "evaluates (equal? 5 5)" $
                evalString "(equal? 5 5)" `shouldBe` Right (BoolValue True)
            it "evaluates (equal? 6 5)" $
                evalString "(equal? 6 5)" `shouldBe` Right (BoolValue False)
            it "evaluates (equal? 5 6)" $
                evalString "(equal? 5 6)" `shouldBe` Right (BoolValue False)
        
        describe "eval If and Let Expressions" $ do
            it "evaluates (if true 5 6)" $
                evalString "(if true 5 6)" `shouldBe` Right (IntValue 5)
            it "evaluates (let (test 5) (if (< test 6) 1 0))" $
                evalString "(let (test 5) (if (< test 6) 1 0))" `shouldBe` Right (IntValue 1)
            it "evaluates (let (test 5) (+ test 5))" $
                evalString "(let (test 5) (+ test 5))" `shouldBe` Right (IntValue 10)

        describe "eval lambda and apply expressions" $ do
            it "evaluates (lambda (x) (+ x 1))" $
                evalString "(lambda (x) (+ x 1))" `shouldBe` Right (ClosureValue "" "x" (MathExpr Add [VarExpr "x",LiteralExpr (IntValue 1)]) [])
            
            it "evaluates (lambda (y) (* y 5))" $
                evalString "(lambda (y) (* y 5))" `shouldBe` Right (ClosureValue "" "y" (MathExpr Mul [VarExpr "y",LiteralExpr (IntValue 5)]) [])
            
            it "evaluates (lambda (y) (let (x 5) (* y x)))" $
                evalString "(lambda (y) (let (x 5) (* y x)))" `shouldBe` Right (ClosureValue "" "y" (LetExpr "x" (LiteralExpr (IntValue 5)) (MathExpr Mul [VarExpr "y",VarExpr "x"])) [])
            
            it "evaluates ((lambda (y) (let (x 5) (* y x))) 4)" $
                evalString "((lambda (y) (let (x 5) (* y x))) 4)" `shouldBe` Right (IntValue 20)

            it "evaluates (let (diff 21) ((lambda (x) (- x 18)) diff))" $
                evalString "(let (diff 21) ((lambda (x) (- x 18)) diff))" `shouldBe` Right (IntValue 3)

            it "evaluates ((lambda (x) (+ x 1)) 2)" $
                evalString "((lambda (x) (+ x 1)) 2)" `shouldBe` Right (IntValue 3)

            it "evaluates (let (f (lambda (x) (+ x 1))) (f 2))" $
                evalString "(let (f (lambda (x) (+ x 1))) (f 2))" `shouldBe` Right (IntValue 3)
            
            it "evaluates (let (f (lambda (x) (if (< x 10) (f (+ x 1)) x))) (f 0))" $
                evalString "(let (f (lambda (x) (if (< x 10) (f (+ x 1)) x))) (f 0))" `shouldBe` Right (IntValue 10)
            
            
module MiniRacketParserSpec where 

    import Test.Hspec
    import Parser
    import Expr 
    import MiniRacketParser
    import Error
    
    type ParseResult = Either ErrorType (Expr, String)

    expr :: Either ErrorType (a2, b) -> a2
    expr (Right (e, _)) = e 
    expr (Left (SyntaxError msg)) = error msg
    expr (Left (ParseError msg)) = error msg
    expr (Left NoParse) = error "no matching parse"
    expr _ = error "expr in MiniRacketParser.hs is not fully implemented yet..."

    spec :: Spec 
    spec = do 
        describe "parse literals" $ do
            it "parses number: 1235" $ 
                parseString "1235" `shouldBe` Right (LiteralExpr (IntValue 1235),"")
            it "parses negative numbers: -12235" $
                parseString "-12235" `shouldBe` Right (LiteralExpr (IntValue (-12235)), "")
            it "parses true" $
                parseString "true" `shouldBe` Right (LiteralExpr (BoolValue True), "")
            it "parses false" $
                parseString "false" `shouldBe` Right (LiteralExpr (BoolValue False), "")
        
        describe "parse bool operations" $ do
            it "parses (and true true)" $
                parseString "(and true true)" `shouldBe` Right (BoolExpr And [LiteralExpr (BoolValue True),LiteralExpr (BoolValue True)], "")
            it "parses (and true false)" $
                parseString "(and true false)" `shouldBe` Right (BoolExpr And [LiteralExpr (BoolValue True),LiteralExpr (BoolValue False)], "")
            it "parses (and false (and true false))" $
                parseString "(and false (and true false))" `shouldBe` Right (BoolExpr And [LiteralExpr (BoolValue False),BoolExpr And [LiteralExpr (BoolValue True),LiteralExpr (BoolValue False)]], "")
            
            it "parses (not false)" $
                parseString "(not false)" `shouldBe` Right (NotExpr (LiteralExpr (BoolValue False)), "")
            it "parses (not true)" $
                parseString "(not true)" `shouldBe` Right (NotExpr (LiteralExpr (BoolValue True)), "")
            
            it "parses (or true false)" $
                parseString "(or true false)" `shouldBe` Right (BoolExpr Or [LiteralExpr (BoolValue True),LiteralExpr (BoolValue False)], "")
            it "parses (or false false)" $
                parseString "(or false false)" `shouldBe` Right (BoolExpr Or [LiteralExpr (BoolValue False),LiteralExpr (BoolValue False)], "")
            it "parses (or true false)" $
                parseString "(or true (not false))" `shouldBe` Right (BoolExpr Or [LiteralExpr (BoolValue True),NotExpr (LiteralExpr (BoolValue False))], "")

        describe "parse math operations" $ do
            it "parses (+ 5 5)" $
                parseString "(+ 5 5)" `shouldBe` Right (MathExpr Add [LiteralExpr (IntValue 5), LiteralExpr (IntValue 5)], "")
            it "parses (+ 2 5 2)" $
                parseString "(+ 2 5 2)" `shouldBe` Right (MathExpr Add [LiteralExpr (IntValue 2), LiteralExpr (IntValue 5),LiteralExpr (IntValue 2)], "")
            it "parses (+ 8 2)" $
                parseString "(+ 8 2)" `shouldBe` Right (MathExpr Add [LiteralExpr (IntValue 8), LiteralExpr (IntValue 2)], "")
            
            it "parses (- 5 1)" $
                parseString "(- 5 1)" `shouldBe` Right (MathExpr Sub [LiteralExpr (IntValue 5), LiteralExpr (IntValue 1)], "")
            it "parses (- 5 1 1)" $
                parseString "(- 5 1 1)" `shouldBe` Right (MathExpr Sub [LiteralExpr (IntValue 5), LiteralExpr (IntValue 1), LiteralExpr (IntValue 1)], "")
            it "parses (- 2 4)" $
                parseString "(- 2 4)" `shouldBe` Right (MathExpr Sub [LiteralExpr (IntValue 2), LiteralExpr (IntValue 4)], "")

            it "parses (* 5 2)" $
                parseString "(* 5 2)" `shouldBe` Right (MathExpr Mul [LiteralExpr (IntValue 5), LiteralExpr (IntValue 2)], "")
            it "parses (* 5 2 2)" $
                parseString "(* 5 2 2)" `shouldBe` Right (MathExpr Mul [LiteralExpr (IntValue 5), LiteralExpr (IntValue 2), LiteralExpr (IntValue 2)], "")
            it "parses (* 5 -2)" $
                parseString "(* 5 -2)" `shouldBe` Right (MathExpr Mul [LiteralExpr (IntValue 5), LiteralExpr (IntValue (-2))], "")
            
            it "parses (div 10 2)" $
                parseString "(div 10 2)" `shouldBe` Right (MathExpr Div [LiteralExpr (IntValue 10), LiteralExpr (IntValue 2)], "")
            it "parses (div 2 2)" $
                parseString "(div 2 2)" `shouldBe` Right (MathExpr Div [LiteralExpr (IntValue 2), LiteralExpr (IntValue 2)], "")
            it "parses (div -8 4)" $
                parseString "(div -8 4)" `shouldBe` Right (MathExpr Div [LiteralExpr (IntValue (-8)), LiteralExpr (IntValue 4)], "")

            it "parses (mod 10 2)" $
                parseString "(mod 10 2)" `shouldBe` Right (MathExpr Mod [LiteralExpr (IntValue 10), LiteralExpr (IntValue 2)], "")
            it "parses (mod 11 2)" $
                parseString "(mod 11 2)" `shouldBe` Right (MathExpr Mod [LiteralExpr (IntValue 11), LiteralExpr (IntValue 2)], "")
            it "parses (mod 3 2)" $
                parseString "(mod 3 2)" `shouldBe` Right (MathExpr Mod [LiteralExpr (IntValue 3), LiteralExpr (IntValue 2)], "")

        describe "parse Compares" $ do
            it "parse (< 5 6)" $
                parseString "(< 5 6)" `shouldBe` Right (CompExpr Lt (LiteralExpr (IntValue 5)) (LiteralExpr (IntValue 6)), "")

            it "parse (equal? 5 5)" $
                parseString "(equal? 5 5)" `shouldBe` Right (CompExpr Eq (LiteralExpr (IntValue 5)) (LiteralExpr (IntValue 5)), "")

        describe "parse if and variable" $ do
            it "parse (if true 5 6)" $
                parseString "(if true 5 6)" `shouldBe` Right (IfExpr (LiteralExpr (BoolValue True)) (LiteralExpr (IntValue 5)) (LiteralExpr (IntValue 6)), "")
            it "parse (let (test 5) (if (< test 6) 1 0))" $
                parseString "(let (test 5) (if (< test 6) 1 0))" `shouldBe` Right (LetExpr "test" (LiteralExpr (IntValue 5)) (IfExpr (CompExpr Lt (VarExpr "test") (LiteralExpr (IntValue 6))) (LiteralExpr (IntValue 1)) (LiteralExpr (IntValue 0))),"")
            it "parse (let (test 5) (+ test 5))" $
                parseString "(let (test 5) (+ test 5))" `shouldBe` Right (LetExpr "test" (LiteralExpr (IntValue 5)) (MathExpr Add [VarExpr "test",LiteralExpr (IntValue 5)]),"")

            it "parse - diff" $
                parseString "- diff" `shouldBe` Right (NegateExpr (VarExpr "diff"),"")
            it "parse - x" $
                parseString "- x" `shouldBe` Right (NegateExpr (VarExpr "x"),"")

        
        describe "parse Lambda and apply Expressions" $ do
            it "parse (lambda (x) (+ x 1))" $
                parseString "(lambda (x) (+ x 1))" `shouldBe` Right (LambdaExpr "x" (MathExpr Add [VarExpr "x",LiteralExpr (IntValue 1)]),"")
            
            it "parse (lambda (y) (* y 5))" $
                parseString "(lambda (y) (* y 5))" `shouldBe` Right (LambdaExpr "y" (MathExpr Mul [VarExpr "y",LiteralExpr (IntValue 5)]),"")
            
            it "parse (lambda (var) (- var 7))" $
                parseString "(lambda (var) (- var 7))" `shouldBe` Right (LambdaExpr "var" (MathExpr Sub [VarExpr "var",LiteralExpr (IntValue 7)]),"")

            it "parse ((lambda (var) (- var 7)) 10)" $
                parseString "((lambda (var) (- var 7)) 10)" `shouldBe` Right (ApplyExpr (LambdaExpr "var" (MathExpr Sub [VarExpr "var",LiteralExpr (IntValue 7)])) (LiteralExpr (IntValue 10)),"")
            
            it "parse ((lambda (x) (+ x 1)) 2)" $
                parseString "((lambda (x) (+ x 1)) 2)" `shouldBe` Right (ApplyExpr (LambdaExpr "x" (MathExpr Add [VarExpr "x",LiteralExpr (IntValue 1)])) (LiteralExpr (IntValue 2)),"")

            it "parse (let (diff 21) ((lambda (x) (- x 18)) diff))" $
                parseString "(let (diff 21) ((lambda (x) (- x 18)) diff))" `shouldBe` Right (LetExpr "diff" (LiteralExpr (IntValue 21)) (ApplyExpr (LambdaExpr "x" (MathExpr Sub [VarExpr "x",LiteralExpr (IntValue 18)])) (VarExpr "diff")),"")

            it "parse (let (f (lambda (x) (if (< x 10) (f (+ x 1)) x))) (f 0))" $
                 parseString "(let (f (lambda (x) (if (< x 10) (f (+ x 1)) x))) (f 0))" `shouldBe` Right (LetExpr "f" (LambdaExpr "x" (IfExpr (CompExpr Lt (VarExpr "x") (LiteralExpr (IntValue 10))) (ApplyExpr (VarExpr "f") (MathExpr Add [VarExpr "x",LiteralExpr (IntValue 1)])) (VarExpr "x"))) (ApplyExpr (VarExpr "f") (LiteralExpr (IntValue 0))),"")

            it "parse (let (f (lambda (x) (+ x 1))) (f 2))" $
                parseString "(let (f (lambda (x) (+ x 1))) (f 2))" `shouldBe` Right (LetExpr "f" (LambdaExpr "x" (MathExpr Add [VarExpr "x",LiteralExpr (IntValue 1)])) (ApplyExpr (VarExpr "f") (LiteralExpr (IntValue 2))),"")

            


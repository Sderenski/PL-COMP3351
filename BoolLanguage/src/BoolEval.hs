module BoolEval where
    import BoolParser
    import BoolDefinitions
    import Environment

    --CompileAndRun
    -- Consumes a string which is the program in the bool langauge
    -- Produce the result of
    --   lexing
    --   parsing
    --   evaluating the program
    compileAndRun :: String -> ValueType
    compileAndRun program = evaluate (parseString program) emptyEnv

    --evaluate
    --  consumes a ParseTree
    --  produces a result value of evaluating the given Parse Tree
    evaluate :: ParseTree -> Environment -> ValueType
    evaluate tree env = case tree of 
        (ValueNode val) -> val
        (IdNode name) -> applyEnv name env
        (NotNode val) -> let param = evaluate val env
                            in
                                case param of
                                    (BoolType True) -> BoolType False
                                    (BoolType False) -> BoolType True
        (AndNode val1 val2) -> let param1 = evaluate val1 env
                                   param2 = evaluate val2 env
                                   in
                                        case param1 of
                                            BoolType True -> param2
                                            BoolType False -> BoolType False
        (OrNode val1 val2) -> let param1 = evaluate val1 env
                                  param2 = evaluate val2 env
                                  in
                                    case param1 of
                                        BoolType True -> BoolType True
                                        BoolType False -> param2
        (LetNode (IdNode param) value body) -> let result = evaluate value env
                                        in
                                            evaluate body (extendEnv (param, result) env)
        (LambdaNode (IdNode name) body) -> ClosureType name body env
        (CallNode (IdNode name) actualParam) -> let actualValue = (evaluate actualParam env)
                                        in
                                            let (ClosureType paramName body funEnv) = applyEnv name env
                                                in
                                                    evaluate body (extendEnv (paramName, actualValue) funEnv) 
        (ParenthesizeNode expr) -> evaluate expr env
        unhandled -> error ("Unmatched Tree Node: " ++ show unhandled)
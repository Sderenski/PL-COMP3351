module BoolParser where

    import BoolLexer

    import BoolDefinitions    
    --A Parser for things
    -- is a function from strings
    --  to list of pairs
    -- of things and strings

    --ParseString
    parseString :: String -> ParseTree
    parseString str = parseTokens (lexString str) 

    parseTokens :: [Token] -> ParseTree
    parseTokens tokens = let (tree, remTokens) = parseExpression tokens
                            in
                                case remTokens of
                                    [] -> tree
                                    _ -> error 
    
    parseExpression :: [Token] -> (ParseTree, [Token])
    parseExpression tokens =
        case lookAhead tokens of
            BoolValueToken val -> (ValueNode (BoolType val), accept tokens)
            NotToken -> let (expr, tokens') = parseExpression (accept tokens)
                            in
                                (NoteNode expr, tokens')
            AndToken -> let (first, tokens') = parseExpression (accept tokens)
                            in
                                let (second, tokens'') = parseExpression tokens'
                                    in
                                        (AndNode first second, tokens'')
            _ -> (EmptyNode, tokens)
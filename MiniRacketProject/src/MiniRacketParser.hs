module MiniRacketParser where

    import Parser
    import Expr
    import Control.Applicative
    import Error ( ErrorType )

    parseBool :: Parser Bool
    parseBool = do
            parseKeyword "true"
            return True
            <|> do
                parseKeyword "false"
                return False


    -- parse binary bool operations
    parseBoolOp :: Parser BoolOp
    parseBoolOp = do
            parseKeyword "and" >> return And
            <|> do
                parseKeyword "or" >> return Or


    -- parse math operations and return the MathOp
    parseMathOp :: Parser MathOp
    parseMathOp =
        do symbol "+" >> return Add
        <|> do symbol "-" >> return Sub
        <|> do symbol "*" >> return Mul
        <|> do parseKeyword "div" >> return Div
        <|> do parseKeyword "mod" >> return Mod


    -- parse the comparison operations and return the corresponding  CompOp
    parseCompOp :: Parser CompOp
    parseCompOp =
        do symbol "<" >> return Lt
        -- <|> do parseKeyword "equal?" >> return Eq

    -- a literal in MiniRacket is true, false, or a number
    literal :: Parser Value
    literal = (IntValue <$> natural) <|> (BoolValue <$> parseBool)



    -- parse a literal expression, which is just a literal
    literalExpr :: Parser Expr
    literalExpr = do
        LiteralExpr <$> literal


    keywordList :: [String]
    keywordList = ["false", "true", "not", "and", "or", "div", "mod", "equal?", "if", "let"]

    -- try to parse a keyword, otherwise it is a variable, this can be
    -- used to check if the identifier we see (i.e., variable name) is
    -- actually a keyword, which is not legal
    parseKeyword :: String -> Parser String
    parseKeyword keyword = do
        -- all keywords follow the identifier rules, so we'll use that
        name <- identifier
        if name `elem` keywordList && keyword == name
            then return name
            else failParse $ "saw " ++ name ++ ", expected " ++ keyword


    -- parse not expressions, note that "not" is a keyword,
    notExpr :: Parser Expr
    notExpr = do
        parseKeyword "not"
        NotExpr <$> parseExpr

    -- a bool expression is the operator followed by one or more expressions
    boolExpr :: Parser Expr
    boolExpr = do
        operation <- parseBoolOp
        values <- oneOrMore parseExpr
        return (BoolExpr operation values)


    -- a math expression is the operator followed by one or more expressions
    mathExpr :: Parser Expr
    mathExpr = do
        operation <- parseMathOp
        val2 <- oneOrMore parseExpr
        return (MathExpr operation val2)



    -- a comparison expression is the comparison operator
    --   followed by two expressions
    compExpr :: Parser Expr
    compExpr = CompExpr <$> parseCompOp <*> parseExpr <*> parseExpr

    pairExpr :: Parser Expr
    pairExpr = do
        expr1 <- parseExpr
        symbol "."
        PairExpr expr1 <$> parseExpr

    -- note that this is syntactic sugar, cons is just replaced by a 
    --    PairExpr abstract syntax tree 
    consExpr :: Parser Expr
    consExpr = do
        symbol "cons"
        expr1 <- parseExpr
        PairExpr expr1 <$> parseExpr

    parseParens :: Parser Expr -> Parser Expr
    parseParens p = do
        symbol "("
        e <- p
        symbol ")"
        return e

    -- the main parsing function which alternates between all
    -- the options you have for possible expressions
    -- TODO: add the additional kinds of things that can be an atom:
    --   an atom is either a var, a literal, or a negated atom
    parseAtom :: Parser Expr
    parseAtom = do
        literalExpr
        <|> varExpr
        <|> negateAtom

    -- TODO: Implement negateAtom
    -- negate an atom, we actually only have one choice here. Our
    -- parsing already correctly handles negative numbers, and we
    -- cannot have negative boolean values. This leaves variables, 
    -- but this needs to build a NegateExpr around the VarExpr.
    negateAtom :: Parser Expr
    negateAtom = do
        symbol "-"
        eatspace
        NegateExpr <$> varExpr

    -- TODO: Implement varExpr
    -- parse a var expression, here we need to make sure that
    -- the identifier is *not* a keyword before accepting it
    -- i.e., we fail the parse if it is     
    varExpr :: Parser Expr
    varExpr = do
        vName <- identifier
        if vName `elem` keywordList
            then failParse $ "Variable " ++ vName ++ " is a keyword"
            else return (VarExpr vName)

    -- TODO: Implement ifExpr
    -- parse an if-expression, which begins with the keyword if,
    -- and is followed by three expressions
    ifExpr :: Parser Expr
    ifExpr = do
        parseKeyword "if"
        IfExpr <$> parseExpr <*> parseExpr <*> parseExpr


    -- TODO: Implement let expressions  
    -- a let expression begins with the keyword let, followed by
    -- left parenthesis, then an identifier for the name 
    -- to be bound, an expression to bind to that name, and a right
    -- parenthesis, and then the body of the let expression
    letExpr :: Parser Expr
    letExpr = do
        parseKeyword "let"
        symbol "("
        -- How do I take the var expr and use it for the name 
        name <- parseExpr
        expr1 <- parseExpr
        symbol ")"
        expr2 <- parseExpr
        case name of 
            VarExpr vName -> return (LetExpr vName expr1 expr2)
            _ -> failParse $ "Name wasn't a valid variable name"
    -- TODO: Implement lambdaExpr 
    -- parse a lambda expression which is a lambda, argument, 
    -- and body, with proper parenthesis around it
    lambdaExpr :: Parser Expr
    lambdaExpr = failParse "not implemented"

    --TODO: Implement applyExpr
    -- This expression consists of a function which is being applied to 
    --   a parameter expression.
    applyExpr :: Parser Expr
    applyExpr = failParse "not implemented"

    -- TODO: Add any newly added kinds of expression to be parsed here
    -- the main parsing function which alternates between all 
    -- the options for possible expressions
    parseExpr :: Parser Expr
    parseExpr = do
        parseAtom
        <|> parseParens notExpr
        <|> parseParens boolExpr
        <|> parseParens mathExpr
        <|> parseParens parseExpr
        <|> parseParens compExpr
        <|> parseParens pairExpr
        <|> parseParens consExpr
        <|> parseParens ifExpr
        <|> parseParens letExpr


    -- a helper function for testing parsing
    --   To use simply type:
    --      parseString "5" 
    --   this will use the parseExpr Parser to parse the contents of str
    parseString :: String -> Either ErrorType (Expr, String)
    parseString str = do
        parse parseExpr str

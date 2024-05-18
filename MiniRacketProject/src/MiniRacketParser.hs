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
        <|> do parseKeyword "equal?" >> return Eq

    -- a literal in MiniRacket is true, false, or a number
    literal :: Parser Value
    literal = (IntValue <$> int) <|> (BoolValue <$> parseBool)



    -- parse a literal expression, which is just a literal
    literalExpr :: Parser Expr
    literalExpr = do
        LiteralExpr <$> literal


    keywordList :: [String]
    keywordList = ["false", "true", "not", "and", "or", "div", "mod", "equal?"]

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


    -- TODO: parse not expressions, note that "not" is a keyword,
    -- (HINT: you should use parseKeyword)
    notExpr :: Parser Expr
    notExpr = do
        parseKeyword "not"
        NotExpr <$> parseExpr

    -- TODO: parse boolean expressions
    -- a bool expression is the operator followed by one or more expressions
    boolExpr :: Parser Expr
    boolExpr = do
        operation <- parseBoolOp
        values <- oneOrMore parseExpr
        return (BoolExpr operation values)


    -- TODO: parse maths expressions
    -- a math expression is the operator followed by one or more expressions
    mathExpr :: Parser Expr
    mathExpr = do
        operation <- parseMathOp
        -- It doesn't seem to eat the space between integer values... Why
        -- Would a case depending on the operation mean that the system could take more values to the param
        val2 <- oneOrMore (eatspace >> parseExpr)
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
    -- TODO: Add new expression types here
    parseExpr :: Parser Expr
    parseExpr = do
        literalExpr
        <|> parseParens parseExpr
        <|> parseParens compExpr
        <|> parseParens pairExpr
        <|> parseParens consExpr
        <|> parseParens notExpr
        <|> parseParens boolExpr
        <|> parseParens mathExpr




    -- a helper function for testing parsing
    --   To use simply type:
    --      parseString "5" 
    --   this will use the parseExpr Parser to parse the contents of str
    parseString :: String -> Either ErrorType (Expr, String)
    parseString str = do
        parse parseExpr str

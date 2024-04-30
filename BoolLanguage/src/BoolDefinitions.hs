module BoolDefinitions where

    data ParseTree = AndNode ParseTree ParseTree |
                        OrNode ParseTree ParseTree |
                        NotNode ParseTree |
                        LetNode ParseTree ParseTree ParseTree |
                        CallNode ParseTree ParseTree |
                        LambdaNode ParseTree ParseTree |
                        ValueNode ValueType |
                        IdNode String |
                        ParenthesizeNode ParseTree |
                        EmptyNode
                            deriving(Show)

    

    
                        
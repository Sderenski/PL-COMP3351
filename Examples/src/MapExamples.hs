module MapExamples where
    -- Higher Order Functions
    --    When a function takes another function as a parameter


    --Map
    --  Example of a 'Higher Order Function"
    -- Takes a function as a parameter and applies it every element in a list
    -- Then produces the outcome of that function with the list
    --      The types of the map function 
    -- map :: (a -> b) -> [a] -> [b]
    -- Example of what is called a Functor (the list is a functor)

    -- sooo if it is map length ["hello", "kermit"] produces [5,6]

    square :: Num a => a -> a
    square x = x * x

    squareMap :: Num a => [a] -> [a]
    squareMap = map square

    squareMap' :: [Integer] -> [Integer]
    squareMap' = map (\n -> n * n)

    doubleString :: [String] -> [String]
    doubleString = map (\s -> s ++ s)

    --Filter
    --  Another Example of 'Higher Order Function'
    -- filter :: (a -> Bool) -> [a] -> [a]

    --fold
    -- Another Example of 'Higher Order Functions'
    -- Two different fold, foldl (Fold left) and foldr (fold right)
    -- takes a list and a few parameters to crush it down to a single value
    -- example: foldl (+) 0 [1,2,3,4,5] -> 15
    -- example: foldl (++) [] ["hello", "bye"] -> "hellobye"
    -- foldl :: (a -> b -> a) -> a -> [a] -> a
    



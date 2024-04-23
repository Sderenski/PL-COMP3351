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

    -- the use of a lambda function
    squareMap' :: [Integer] -> [Integer]
    squareMap' = map (\n -> n * n)

    doubleString :: [String] -> [String]
    doubleString = map (\s -> s ++ s)

    zipList :: [Integer] -> [Integer] -> [(Integer, Integer)]
    zipList [] [] = []
    zipList _ [] = []
    zipList [] _ = []
    zipList (x:xs) (y:ys) = (x,y) : zipList xs ys

    unzipList :: [(Integer, Integer)] -> ([Integer], [Integer])
    unzipList x = ( [a | (a,_) <- x],
                    [b | (b,_) <-x] )

    unzipList' x = ( map fst x, map snd x)

    unzipList'' x = ( map (\(a,b) -> a) x, map (\(a,b) -> b) ) 

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
    
    boolAnd :: [Bool] -> Bool
    boolAnd [] = True
    boolAnd (b:bs) = b && (boolAnd bs)

    boolAnd' :: [Bool] -> Bool
    boolAnd' = foldl (&&) True 

    boolAnd'' :: [Bool] -> Bool
    boolAnd'' = foldr (&&) True

    addList :: (Num a) => [a] -> a
    addList = foldl (+) 0

    min' :: (Num a, Ord a) => [a] -> Maybe a
    min' [] = Nothing
    min' [x] = Just x
    min' (x:xs) = let result = min' xs
                    in 
                        case result of
                            (Just small) -> if x <= small then (Just x) else (Just small)
                            Nothing -> Just x



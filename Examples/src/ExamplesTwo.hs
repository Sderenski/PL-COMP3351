module ExamplesTwo where

    -- [] empty list
    -- [1,2,3] the list containing 1 , 2 , 3
    -- cons operator
    -- Cons operator has a low prioty compared to other operators

    -- Write comments first, then test cases for the code, then finally actually the functions
    -- Make sure you cover all the possible cases

    -- Everything in this language is immutable. You have to declare new variables and lists. Have to build new lists if they need to be edited

    len :: Num p => [a] -> p
    len [] = 0
    len (q : qs) = 1 + len qs

    doubleList :: [Integer] -> [Integer]
    doubleList [] = []
    doubleList (q : qs) = (2 * q) : doubleList qs 

    squareList :: [Integer] -> [Integer]
    squareList [] = []
    squareList (q : qs) = (q * q) : squareList qs
    
    -- More General idea of the two above, abstract version
    -- in other words trying to pass a function in as a parameter
    modifyList :: (Integer -> Integer) -> [Integer] -> [Integer]
    modifyList f [] = []
    modifyList f (q : qs) = (f q) : modifyList f qs

    greaterThan2 :: [Integer] -> [Integer]
    greaterThan2 [] = []
    greaterThan2 (q : qs) = if q > 2 
                                then q : greaterThan2 qs
                                else greaterThan2 qs
    

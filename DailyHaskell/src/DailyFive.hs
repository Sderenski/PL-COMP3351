module DailyFive where
    import Data.Char (isLower)

    --multPairs
    --  Consumes a list of pairs of integers
    --  produces a list of products of each pair
    multPairs :: [(Integer, Integer)] -> [Integer]
    multPairs [] = []
    multPairs list = map (\(x,y) -> x * y) list


    --squareList
    --  Consumes a list of integers
    --  produces a new list of pairs of integers
    squareList :: [Integer] -> [(Integer, Integer)]
    squareList [] = []
    squareList list = map (\n -> (n, n*n)) list

    --findLowercase
    --  Takes in a list of Strings
    --  Consumes a list of Bool
    findLowercase :: [String] -> [Bool]
    findLowercase [] = []
    findLowercase list = map (\str -> isLower (head str)) list
    
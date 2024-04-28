module DailySix where

    --shorterThan
    -- consumes a number and a list of words
    -- produce a list of words whose length is shorter than or equal to the given number
    shorterThan :: Int -> [String] -> [String]
    shorterThan _ [] = []
    shorterThan 0 _ = []
    shorterThan bound theList = filter (\x -> length x <= bound) theList


    --removeMultiple
    --  consumes a number and a list of numbers
    --  produce a list where the multiples of the given number have been removed.
    removeMultiple :: Integer -> [Integer] -> [Integer]
    removeMultiple _ [] = []
    removeMultiple value theList = filter (\x -> x `mod` value /= 0) theList

    --onlyJust
    -- consumes a list of Maybe a
    -- produce a list where all values of Nothing have been eliminated
    onlyJust :: [Maybe a] -> [Maybe a]
    onlyJust [] = []
    onlyJust [Nothing] = []
    onlyJust theList = filter (\case {Nothing -> False; Just _ -> True}) theList
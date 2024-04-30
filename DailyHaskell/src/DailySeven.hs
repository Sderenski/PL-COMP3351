module DailySeven where

    --findLongest
    --  Consumes a list of words
    --  Produce first that is longer than or equal 
    --    to the length of each other word in list
    findLongest :: [String] -> String
    findLongest [] = ""
    findLongest (x:xs) = foldl (\highest word -> if length highest >= length word then highest else word) x xs


    --anyLarger
    --  consumes an integer and a list of integers
    --  produce True if any of the integers in the list are 
    --     equal or larger than the input
    anyLarger :: Integer -> [Integer] -> Bool
    anyLarger _ [] = False
    anyLarger val list = foldl (\prevBool x -> prevBool || x >= val) False list


    --allNames
    --  consumes a list of tuples which each 
    --      contain a first name and last name
    --  Produces a single string which contains all the first names 
    --      and last names seperated by commas
    allNames :: [(String, String)] -> String
    allNames [] = ""
    allNames names = foldl (\x (first, lastName) -> x ++ if null x then first ++ " " ++ lastName else ", " ++ first ++ " " ++ lastName) "" names

         

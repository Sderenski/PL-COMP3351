{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module DailyTwo where

    --every4th
    --  Takes a list
    --  Produces a list
    --      only consists of every fourth element
    every4th :: [a] -> [a]
    every4th [] = []
    every4th (a:b:c:d:es) = d : every4th es
    every4th _ = []

    -- tupleDotProduct takes two lists and finds the dot product between them 
    -- takes two lists
    -- produces a number
    tupleDotProduct :: [Integer] -> [Integer] -> Integer
    tupleDotProduct [] [] = 0
    tupleDotProduct (q : qs) (p : ps) = (q * p) + tupleDotProduct qs ps


    --appendToEach
    --  Takes a string and list of strings
    --  produces a list of strings with the string appended to each other
    appendToEach :: [Char] -> [[Char]] -> [[Char]]
    appendToEach f [] = []
    appendToEach f (q : qs) = (q ++ f) : appendToEach f qs

    
    --toSetList
    --  Takes a list
    --  returns an ordered list
    --      Uses quick sort from class to order the return list
    toSetList :: Ord a => [a] -> [a]
    toSetList [] = []
    toSetList (q:qs) = quicksort(q : toSetList (removeDuplicates q qs))
                 
    --removeDuplicates
    --   takes an element and list
    --   produces a list 
    removeDuplicates :: (Eq a) => a -> [a] -> [a]
    removeDuplicates f [] = []
    removeDuplicates f (w:ws) =  if f == w 
                                    then removeDuplicates f ws 
                                    else w : removeDuplicates f ws

    --QuickSort
    --  Consume a list of Integer
    --  Produce a sorted list of the given Integers
    quicksort :: Ord a => [a] -> [a]
    quicksort [] = []
    quicksort (q : qs) = let (smaller, larger) = split q qs
                            in 
                                (quicksort smaller) ++ [q] ++ (quicksort larger)

    -- Helper Functions for QuickSort

    --Split
    --  Consumes an element and a list
    --  Produces two lists: everything less than element and everything more than element
    split :: Ord a => a -> [a] -> ([a], [a])
    split pivot [] = ([],[])
    split pivot (q : qs) = let (smaller, larger) = split pivot qs
                            in 
                                if q < pivot
                                    then (q : smaller, larger)
                                    else (smaller, q : larger)

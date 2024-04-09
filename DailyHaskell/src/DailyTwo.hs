module DailyTwo where

    -- This is where the issues need to be for the next few functions

    -- Function every4th
    -- Takes a list and constructss a new list only consisiting of every fourth 
    -- element. Thus no loops, only recussively.
    -- I am lost without and index, maybe there is a way to keep 
    -- track of which element index you are one simply from the elment??
    every4th :: [a1] -> [a2]
    every4th [] = []

    -- tupleDotProduct takes two lists and finds the dot product between them 
    tupleDotProduct :: [Integer] -> [Integer] -> Integer
    tupleDotProduct [] [] = 0
    tupleDotProduct (q : qs) (p : ps) = (q * p) + tupleDotProduct qs ps

    appendToEach :: [Char] -> [[Char]] -> [[Char]]
    appendToEach f [] = []
    appendToEach f (q : qs) = (q ++ f) : appendToEach f qs

    -- How do you remove the duplicates in a list recrusively?
    -- The only way I can think through this one is more than a single line
    -- Most things are using other built-in or imported functions to solve this issue
    toSetList :: [a1] -> [a2]
    toSetList [] = []

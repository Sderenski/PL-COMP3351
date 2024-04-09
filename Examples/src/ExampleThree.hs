{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module ExampleThree where

    -- "One of" or "Some of" type
    -- Empty is a constructor
    -- or Node is a constructor of it too
    data MyList = Empty | Node Integer MyList
        deriving(Show) -- be a member of the type class

    -- 2 different constructors
    data MyBool = MyTrue | MyFalse

    -- Creating your own binary search tree
    data MyTree = TreeEmpty | TreeNode Integer MyTree MyTree
        deriving(Show)


    myElem :: Integer -> MyList -> Bool
    myElem _ Empty = False
    myElem val (Node v theRest) = val == v || myElem val theRest

    myLength :: MyList -> Integer
    myLength Empty = 0
    myLength (Node v theRest) = 1 + myLength theRest

    -------------------------------------------------------------------------------------------------------------------------------

    --QuickSort
    --  Consume a list of Integer
    --  Produce a sorted list of the given Integers
    quicksort [] = []
    quicksort (q : qs) = let (smaller, larger) = split q qs
                            in 
                                (quicksort smaller) ++ [q] ++ (quicksort larger)

    -- Helper Functions for QuickSort

    --Split
    --  Consumes an element and a list
    --  Produces two lists: everything less than element and everything more than element
    split pivot [] = ([],[])
    split pivot (q : qs) = let (smaller, larger) = split pivot qs
                            in 
                                if q < pivot
                                    then (q : smaller, larger)
                                    else (smaller, q : larger)

    -----------------------------------------------------------------------------------------------------
    -- What is going on with the different types of the function???

    -- MergeSort
    --   Consumes a list of MergeSort
    --   Produce a sorted list of the given Integers
    mergeSort [] = []
    mergeSort [q] = [q]
    mergeSort [q,r] = if q < r then [r,q] else [q,r]
    mergeSort theList = let lowerList = take (length theList)/2 theList
                            upperList = drop (length theList)/2 theList
                        in
                            merge (mergeSort lowerList) (mergeSort upperList)

    -- Helper Function for MergeSort

    --Merge
    --  Consume two in order lists of integers
    --  produces one in order list of integers
    merge [] [] = []
    merge (a:as) [] = a:as
    merge [] (b:bs) = b:bs
    merge (a:as) (b:bs) = if a < b
                            then a : merge as (b:bs)
                            else b: merge (a:as) bs
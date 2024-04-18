module DailyFour where

    --zip3Lists
    --   takes three lists 
    --   produces a list of 3 element tuples
    zip3Lists :: [a] -> [b] -> [c] -> [(a, b, c)]
    zip3Lists [] [] [] = []
    zip3Lists (q:qs) (e:es) (w:ws) = (q, e, w) : zip3Lists qs es ws

    --unzipTriples
    --   Takes a list of triples
    --   produces a tuple of three lists
    unzipTriples :: [(a,b,c)] -> ([a],[b],[c])
    unzipTriples [] = ([], [], [])
    unzipTriples ((a,b,c):qs) = let (xs, ys, zs) = unzipTriples qs
                    in 
                        (a : xs, b : ys, c : zs)

    --mergeSorted3
    --   Takes 3 lists
    --   produces a merged sorted list
    mergeSorted3 :: Ord a => [a] -> [a] -> [a] -> [a]
    mergeSorted3 [] [] [] = []
    mergeSorted3 (q:qs) [] [] = q:qs
    mergeSorted3 [] (w:ws) [] = w:ws
    mergeSorted3 [] [] (e:es) = e:es
    mergeSorted3 first second [] = merge first second
    mergeSorted3 first [] third = merge first third
    mergeSorted3 [] second third = merge second third
    mergeSorted3 firstList secondList thirdList = let simplifiedList = merge firstList secondList
                            in 
                                merge simplifiedList thirdList

    --merge
    --  Takes two lists
    --  Produces a list
    --    used to make the mergeSorted 3 lists into just two
    merge :: Ord a => [a] -> [a] -> [a]
    merge [] [] = []
    merge (a:as) [] = a:as
    merge [] (b:bs) = b:bs
    merge (a:as) (b:bs)
        | a < b = a : merge as (b:bs)
        | otherwise = b : merge (a:as) bs
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
    unzipTriples :: [(a,b,c)] -> ([a,b,c])
    unzipTriples [] = []
    unzipTriples ((t:ts):qs) = []

    --mergeSorted3
    --   Takes 3 lists
    --   produces a merged sorted list
    mergeSorted3 :: Ord a => [a] -> [a] -> [a] -> [a]
    mergeSorted3 [] [] [] = []
    mergeSorted3 (q:qs) (w:ws) (e:es) = if q < w 
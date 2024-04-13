module DailyThree where

    --removeAllExpect
    --   Takes an element and list
    --   produces a list
    removeAllExpect :: Eq a => a -> [a] -> [a]
    removeAllExpect e [] = []
    removeAllExpect e (q:qs) = if e == q 
                                    then q : removeAllExpect e qs
                                    else removeAllExpect e qs

    --countOccurences
    --   Takes an element and list
    --   produces a number telling how many times something was in a list
    countOccurences :: Eq a => a -> [a] -> Int
    countOccurences e [] = 0
    countOccurences e (q:qs) = if e == q
                                    then 1 + countOccurences e qs
                                    else countOccurences e qs

    --substitute
    --   Take two elements and a list
    --   produce a list
    --      the two elements are that of what is being replaced vs what isn't
    substitute :: Eq a => a -> a -> [a] -> [a]
    substitute r n [] = []
    substitute r n (q:qs) = if r == q
                                then n : substitute r n qs
                                else q : substitute r n qs

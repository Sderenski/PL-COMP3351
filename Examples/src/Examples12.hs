module Examples12 where 
    -- Standard Pure
    pairs :: [a] -> [b] -> [(a,b)]
    pairs [] [] = []
    pairs (x:xs) (q:qs) = (x, q) : pairs xs qs

    -- Catergiory Mathematics with do 
    pairs' :: [a] -> [b] -> [(a,b)]
    pairs' xs ys = do
        x <- xs
        y <- ys
        return (x,y)

    -- List Comprehension
    pairs'' :: [a] -> [b] -> [(a,b)]
    pairs'' xs ys = [(x,y) | x <- xs, y <- ys]

    -- IO in Haskell
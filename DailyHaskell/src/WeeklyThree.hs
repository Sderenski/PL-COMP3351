module WeeklyThree where

    --vec
    --  Data type containing a list of doubles
    --  Derive show
    data Vec = Vec [Double]

    --Instance Show
    instance Show Vec where
        show :: Vec -> String
        show (Vec xs) = "Vec " ++ show xs

    --Instance Num
    -- Need +, -, *, abs, signum, and from Integer
    instance Num Vec where
        --(+)
        --  Takes two Vecs
        --  Produces a Vec
        (+) :: Vec -> Vec -> Vec
        (Vec xs) + (Vec ys) = Vec (zipWith (+) xs ys)

        --(-)
        --  Takes two Vecs
        --  Produces a Vec
        (-) :: Vec -> Vec -> Vec
        (Vec xs) - (Vec ys) = Vec (zipWith (-) xs ys)

        --(*)
        --  Takes two Vecs
        --  Produces a Vec
        (*) :: Vec -> Vec -> Vec
        (Vec xs) * (Vec ys) = Vec (zipWith (*) xs ys)

        --abs
        --  Takes one Vec
        --  Produces a Vec
        abs :: Vec -> Vec
        abs (Vec xs) = Vec (map abs xs)

        --signum
        --  Takes one Vec
        --  Produces a Vec
        signum :: Vec -> Vec
        signum (Vec xs) = Vec (map signum xs)

        --fromInteger
        --  Takes one Integer
        --  Produces a Vec
        fromInteger :: Integer -> Vec
        fromInteger n = Vec (repeat (fromInteger n))

    --Instance Eq
    -- Only need == function
    --  (==) consumes two Vec Lists and produces a Boolean
    instance Eq Vec where
        (==) :: Vec -> Vec -> Bool
        (Vec xs) == (Vec ys) = and (zipWith (==) xs ys)

    --Intance Ord
    --  need <= function
    --  compare consumes two vec lists and produces a boolean
    instance Ord Vec where 
        compare :: Vec -> Vec -> Ordering
        compare (Vec xs) (Vec ys) = compare xs ys

    class VecT a where
        magnitude :: VecT a => a -> Double
    
    instance VecT Vec where
        magnitude :: VecT Vec => Vec -> Double
        magnitude (Vec xs) = sqrt (sum (map (^2) xs))

    instance Semigroup Vec where
        (<>) :: Vec -> Vec -> Vec
        (Vec xs) <> (Vec ys) = Vec (zipWith (+) xs ys)

    instance Monoid Vec where
        mempty :: Vec
        mempty = Vec (repeat 0)
        
        

    
        
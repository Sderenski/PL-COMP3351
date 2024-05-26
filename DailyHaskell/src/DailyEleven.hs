module DailyEleven where

    --firstFunctorLaw
    --  Returns true if the first functor law holds for a
    --  fuctor value
    firstFunctorLaw :: (Eq (f a), Functor f) => f a -> Bool
    firstFunctorLaw x = fmap id x == x

    --secondFunctorLaw
    --  Returns true if the second functor law holds for two 
    --  functions and a functor that are passed to it
    secondFunctorLaw :: (Eq (f c), Functor f) => (b -> c) -> (a -> b) -> f a -> Bool
    secondFunctorLaw g h x = fmap (g . h) x == (fmap g . fmap h) x


    --Functions for testing the functor laws

    --f
    --   Maybe integer to maybe integer by adding plus one to the number
    f :: Maybe Integer -> Maybe Integer
    f (Just x) = Just (x + 1)
    f Nothing = Nothing

    --g
    --  Maybe Integer to maybe integer by multipling by 2 to the number
    g :: Maybe Integer -> Maybe Integer
    g (Just x) = Just (x * 2)
    g Nothing = Nothing
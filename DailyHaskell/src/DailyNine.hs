module DailyNine where

    --onlyNothing
    --   Takes a function and the second parameter is a list
    --   produce True if the first parameter is applied to the 
    --       elements of the second parameter and produces nothing
    --       for each element. Otherwise false
    onlyNothing :: (a -> Maybe b) -> [a] -> Bool
    onlyNothing _ [] = True
    onlyNothing f (q:qs) =
        case f q of
            Nothing -> onlyNothing f qs
            _ -> False


    --firstAnswer
    --  Takes in a function and a list
    --  the first parameter should be applied to elements of the
    --    second parameter in order until the first time it produces
    --    Just v for some v and then Just v is the result of firstAnswer
    --  The first parameter can be thought of as a predicate function.
    --  Rather than returning False or True, It returns Nothing or Just v. 
    firstAnswer :: (a -> Maybe b) -> [a] -> Maybe b
    firstAnswer _ [] = Nothing
    firstAnswer f (q:qs) =
        case f q of 
            Nothing -> firstAnswer f qs
            (Just v) -> Just v


    --allAnswers
    --  First parameter should be applied to elements of the second param
    --  If this produes Nothing for any element, then the result is Nothing.
    --  Otherwise, the first param applied to the elements of the second param
    --     the result would be Just lst.
    allAnswers :: (a -> Maybe [b]) -> [a] -> Maybe [b]
    allAnswers _ [] = Just []
    allAnswers f (q:qs) = 
        case f q of
            Nothing -> Nothing
            Just lst -> case allAnswers f qs of
                           Nothing -> Nothing
                           Just acc -> Just (lst ++ acc)  

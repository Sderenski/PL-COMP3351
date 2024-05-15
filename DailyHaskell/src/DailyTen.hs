module DailyTen where

    --allLefts
    --  should consume a list of Either types
    --  Produce a list of any Left values
    allLefts :: [Either a b] -> [a]
    allLefts [] = []
    allLefts (x:xs) = case x of 
            (Left a) -> a : allLefts xs
            _ -> allLefts xs  

    --produceStringOrSum
    --  COnsumes two Either types
    --  Produce a String if either parameter is a String
    --      the produced string should be the first String parameter if both
    --      parameters are Strings
    --  Produce the sum of the two parameters if they are both integers
    produceStringOrSum :: (Either String Integer) -> (Either String Integer) -> (Either String Integer)
    produceStringOrSum (Left a) (Left b) = Left a
    produceStringOrSum (Left a) _ = Left a
    produceStringOrSum _ (Left b) = Left b
    produceStringOrSum (Right a) (Right b) = Right (a + b)


    --sumListOfEither
    --   Consume a list consisting of Either Strings or Integers
    --   Produce the first String in the list if there is one in the list or the sum
    --       of all the integers in the list if there are no Strings in the given list
    sumListOfEither :: [Either String Integer] -> Either String Integer
    sumListOfEither [] = Right 0
    sumListOfEither (x:xs) = case x of
        (Left a) -> Left a
        (Right b) -> case sumListOfEither xs of
            (Left a2) -> Left a2
            (Right b2) -> Right (b + b2)
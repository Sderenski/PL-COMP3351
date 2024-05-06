module DailyEight where

    --findSmallest
    --  Consume a list
    --  produce Nothing if the list is empty and Just v otherwise
    findSmallest :: (Ord a) => [a] -> Maybe a
    findSmallest [] = Nothing
    findSmallest [q] = Just q
    findSmallest (q:qs) = let result = findSmallest qs
                            in
                                case result of
                                    (Just small) -> if q < small then Just q else Just small
                                    Nothing -> Just q

    --allTrue
    --  Consume a list
    --  produce Nothing if the list is empty,
    --       Just True if all the elements are True
    --       Just False otherwise
    allTrue :: [Bool] -> Maybe Bool
    allTrue [] = Nothing
    allTrue [True] = Just True
    allTrue [False] = Just False
    allTrue (q:qs) = let result = allTrue qs
                        in
                            case result of
                                (Just prev) -> if q && prev then Just True else Just False
                                Nothing -> Just q


    --countAllVotes
    --  consume a list of Maybe Bool
    --  return a tuple with the number of representives who have
    --      not yet voted, voted in favour, and voted against
    countAllVotes :: [Maybe Bool] -> (Integer, Integer, Integer)
    countAllVotes [] = (0,0,0)
    countAllVotes (q:qs) = let (notVoted, votedFavor, votedAgainst) = countAllVotes qs
                                in
                                    case q of
                                        (Just True) -> (notVoted, votedFavor+1, votedAgainst)
                                        (Just False) -> (notVoted, votedFavor, votedAgainst+1)
                                        Nothing -> (notVoted + 1, votedFavor, votedAgainst)

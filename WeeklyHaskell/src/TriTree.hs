module TriTree where

    data TriTree a = Empty |
                    Leaf a |
                    Node a a (TriTree a) (TriTree a) (TriTree a)
                    deriving (Eq, Show)
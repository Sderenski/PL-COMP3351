module TriTree where

    -- Tree is organized:
    --   a tree with no values is simply the Empty Tree
    --   a tree storing exactly one data value is a Leaf with the single value
    --   a tree storing exactly two data values is a Node with the two 
    --      data values in ascending order and three empty subtrees as children
    -- a tree storing more than two data values is some combination of these nodes

    -- The left subtree has values less than or equal to the first value. The 
    -- values in the middle tree are between the two values (strictly greater 
    -- than the first value and less than or equal to the second value), and the 
    -- values in the right subtree are strictly greater than the second value. 

    data TriTree a = Empty |
                    Leaf a |
                    Node a a (TriTree a) (TriTree a) (TriTree a)
                    deriving (Eq, Show)

    --search
    -- consumes a value and a TriTree. 
    -- produce True if the value is stored in the TriTree
    --    False otherwise.
    search :: Ord a => a -> TriTree a -> Bool
    search _ Empty = False
    search value (Leaf v) = value == v
    search value (Node v1 v2 leftTree midTree rightTree)
        | value == v1 = True
        | value == v2 = True
        | value < v1 = search value leftTree
        | value > v2 = search value rightTree
        | otherwise = search value midTree

    --insert
    --  Consumes a value and a TriTree
    --  Produce a new TriTree which contains the give value
    insert :: (Ord a) => a -> TriTree a -> TriTree a
    insert value Empty = Leaf value
    insert value (Leaf v) = if value <= v
                                then Node value v Empty Empty Empty
                                else Node v value Empty Empty Empty
    insert value (Node v1 v2 leftTree midTree rightTree) 
        | value < v1 = Node v1 v2 (insert value leftTree) midTree rightTree
        | value > v2 = Node v1 v2 leftTree midTree (insert value rightTree)
        | otherwise = Node v1 v2 leftTree (insert value midTree) rightTree

    --insertList
    --  Consume a list of values and a TriTree
    --  Produce a new TriTree which contains all of the values from the given list
    insertList :: (Ord a) => [a] -> TriTree a -> TriTree a
    insertList list tree = foldr insert tree list

    --identical
    --  Consumes two TriTrees
    --  Produce True if they are exactly identical
    --  False Otherwise
    identical :: (Eq a) => TriTree a -> TriTree a -> Bool
    identical Empty Empty = True
    identical (Leaf x) (Leaf y) = x == y
    identical (Node x1 y1 leftTree1 midTree1 rightTree1) (Node x2 y2 leftTree2 midTree2 rightTree2) =
        x1 == x2 && y1 == y2 && identical leftTree1 leftTree2 && identical midTree1 midTree2 && identical rightTree1 rightTree2
    identical _ _ = False

    --treeMap
    --  consumes a function, f :: a -> b, and a TriTree
    --  produce a new tree with f being applied to each value stored in the TriTree
    treeMap :: (a -> b) -> TriTree a -> TriTree b
    treeMap _ Empty = Empty 
    treeMap f (Leaf value) = Leaf (f value)
    treeMap f (Node v1 v2 leftTree midTree rightTree) = Node (f v1) (f v2) (treeMap f leftTree) (treeMap f midTree) (treeMap f rightTree)

    --treeFoldPreOrder
    --  Consume a function, f :: a -> b -> a, an inital value, and a TriTree
    --  Produce the result of using f to combine values in the TriTree
    --    Left-most value in the node, then the right-most, then the left, middle, and right subtrees
    treeFoldPreOrder :: (a -> b -> a) -> a -> TriTree b -> a
    treeFoldPreOrder _ value Empty = value
    treeFoldPreOrder f value (Leaf v) = f value v
    treeFoldPreOrder f value (Node v1 v2 left mid right) = 
        let value' = f (f value v1) v2
            value'' = treeFoldPreOrder f value' left
            value''' = treeFoldPreOrder f value'' mid
        in treeFoldPreOrder f value''' right

    --treeFoldInOrder
    --  Consume a function, f :: a -> b -> a, an initial value, and a TriTree
    --  Produce the result of using f to combine values in the TriTree.
    --    Values in the left subtree should be processed first, then the left most value,
    --    followed by middle subtree right-most value then right subtree
    treeFoldInOrder :: (a -> b -> a) -> a -> TriTree b -> a
    treeFoldInOrder _ value Empty = value
    treeFoldInOrder f value (Leaf v) = f value v
    treeFoldInOrder f value (Node v1 v2 left mid right) = 
        let value' = f (treeFoldInOrder f value left) v1
            value'' = treeFoldInOrder f value' mid
            value''' = f value'' v2
        in treeFoldInOrder f value''' right

    --treeFoldPostOrder
    --  Consume a function, f :: a -> b -> a, an initial value, and a TriTree
    --  Produce the result of using f to combine values in the TriTree
    --    Values in the subtrees should be processed first, followed by the left and right values stored in the node.
    treeFoldPostOrder :: (a -> b -> a) -> a -> TriTree b -> a
    treeFoldPostOrder _ value Empty = value
    treeFoldPostOrder f value (Leaf v) = f value v
    treeFoldPostOrder f value (Node v1 v2 left mid right) = 
        let value' = treeFoldPostOrder f value left
            value'' = treeFoldPostOrder f value' mid
            value''' = treeFoldPostOrder f value'' right
        in f (f value''' v1) v2
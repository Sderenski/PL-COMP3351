module ExamplesFour where

    -- Implementation of Binary Search Tree

  data BinTree a = EmptyTree
              | Leaf a
              | Node a (BinTree a) (BinTree a)
              deriving(Show)

  -- Insert a Value
  --    Consume a BinTree and a value
  --    Produce a BinTree with the value inserted
  insert :: (Ord a) => a -> BinTree a -> BinTree a
  insert value EmptyTree = Leaf value
  insert value (Leaf v) = if value <= v
                              then Node v (Leaf value) EmptyTree
                              else Node value EmptyTree (Leaf v)
  insert value (Node v leftTree rightTree) = if value <= v
                                                  then (Node v (insert value leftTree) rightTree)
                                                  else (Node v leftTree (insert value rightTree))

  -- Search for a value
  --   Consume a BinTree and a value
  --   Produce a Boolean
  search :: (Ord a) => a -> BinTree a -> Bool
  search _ EmptyTree = False
  search value (Leaf v) = value == v
  search value (Node v leftTree rightTree)
    | value == v = True
    | value < v = search value leftTree
    | otherwise = search value rightTree

  treeMap :: (a -> b) -> BinTree a -> BinTree b
  treeMap _ EmptyTree = EmptyTree
  treeMap f (Leaf value) = (Leaf (f value))
  treeMap f (Node value leftChild rightChild) = (Node (f value) (treeMap f leftChild) (treeMap f rightChild))
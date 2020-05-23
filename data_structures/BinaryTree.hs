{- 
Author: Russell Rivera
File: binaryTree.hs
Desc: A binary tree implementation
-}

module BinaryTree where

-- polymorphic binary tree
-- Branch :: Tree a -> Tree a -> Tree a -> Tree a
--          ^node     ^right    ^left      ^node with left and right

data BinTree a = Empty
             | Branch a (BinTree a) (BinTree a)
             deriving (Eq, Show)

-- binary search over "orderable things"
-- look at the structure of the recursive calls 
binSearch :: (Ord a) => a -> BinTree a -> Bool
binSearch x Empty = False
binSearch x (Branch node left right)
          | x == node = True
          | x < node = binSearch x right
          | x > node = binSearch x left



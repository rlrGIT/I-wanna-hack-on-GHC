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
             deriving (Eq, Show) -- "inherit" comparability, and repr

-- binary search over "orderable things"
-- look at the structure of the recursive calls 
binSearch :: (Ord a) => a -> BinTree a -> Bool
binSearch x Empty = False
binSearch x (Branch node left right)
          | x == node = True
          | x < node = binSearch x right
          | x > node = binSearch x left

-- insertion on binary trees
-- notice the structure of calling on a subtree
insert :: (Ord a) => a -> BinTree a -> BinTree a
insert x Empty = Branch x Empty Empty 
insert x (Branch node left right)
          | x == node = Branch x right left
          | x < node = Branch node (insert x right) left
          | x > node = Branch node right (insert x left)

-- takes an array of elements, constructs a binary tree
arrayBuild :: (Ord a) => [a] -> BinTree a -> BinTree a
arrayBuild [] Empty = Empty
arrayBuild [] (Branch node left right) = (Branch node left right)
arrayBuild [x] (Branch node left right) = insert x (Branch node left right)
arrayBuild (x:xs) Empty = arrayBuild xs (insert x Empty)
arrayBuild (x:xs) (Branch node left right) = arrayBuild xs (insert x (Branch node left right))


-- delete a node in a binary tree
-- delete :: (Ord a) => a -> BinTree a -> Bintree a


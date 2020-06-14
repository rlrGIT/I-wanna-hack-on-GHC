{-
Name: Russell Rivera
File: Sorting.hs
Desc: Bunch'a sorting stuff
and traversal stuff

-}


module Sorting where

-- bubble sort
switch :: Ord a => [a] -> [a] -- pass through a list, switching elements if that given element is larger then the following element
switch []  = []
switch [x] = [x]
switch (f:s:rest) 
  | f > s     = s: switch (f:rest)
  | otherwise = f: switch (s:rest)

-- do the switch operation n times
multiSwitch :: Ord a => Int -> [a] -> [a]
multiSwitch _ [] = []
multiSwitch 0 xs = xs 
multiSwitch n xs = multiSwitch decr switched
  where
   switched = switch xs
   decr     = (n - 1)

bubble :: Ord a => [a] -> [a]
bubble xs = multiSwitch len xs
  where
      len = length xs 



-- selection sort
select :: Ord a => a -> [a] -> (a, [a]) -- select the minimum value between a list and a given value, return the smallest value, and a list of other numbers in a tuple
select a []              = (a, [])
select a (x:xs)
 | x <= minimum (a:x:xs) = (x, a:xs)
 | otherwise             = select x (xs ++ [a])

selectionSort :: Ord a => [a] -> [a] -- make one switch per pass moving a smaller number to the front of the list until sorted
selectionSort []     = []
selectionSort (x:xs) = a : selectionSort b -- merge the elements of the tuple output of the select function
 where
  (a,b)              = select x xs 


-- insertion sort
insert :: Ord a => a -> [a] -> [a] -- given an value and a list of values, insert the new value in order into the list of values
insert a []             = [a]
insert a (x:xs)
 | a > x && xs == []    = x:xs ++ [a]
 | a < x                = [a] ++ x:xs
 | a > x && a < head xs = [x] ++ [a] ++ xs
 | otherwise            = [x] ++ insert a xs

insertionSort :: Ord a => [a] -> [a] -- sort items into a sublist at the front of the given list until the list is sorted
insertionSort []     = []
insertionSort (x:xs) = insert x (insertionSort xs)



-- merge sort
merge :: Ord a => [a] -> [a] -> [a] -- merge two lists so that the output is a list in order
merge [] []     = []
merge (x:xs) [] = (x:xs) 
merge [] (y:ys) = (y:ys)
merge (x:xs) (y:ys)
 | x <= y       = [x] ++ merge xs (y:ys)
 | otherwise    = [y] ++ merge (x:xs) ys

split :: [a] -> ([a], [a]) -- split one list so that the two sublists produced are no more than one value larger or smaller in length
split []     = ([],[])
split (x:xs) = (take aboutHalf (x:xs), drop aboutHalf (x:xs))
 where
  aboutHalf  = length (x:xs) `div` 2

mergeSort :: Ord a => [a] -> [a] -- split the list down into individual values, then merge them together in order
mergeSort []     = []
mergeSort [x]    = [x]
mergeSort x = merge (mergeSort a) (mergeSort b)
 where
 (a,b)           = split x



-- quicksort, does not use list comprehension
partition :: Ord a => a -> [a] -> ([a], [a]) -- create a tuple containing a list of elements less than or equal to the pivot and one list of elements larger than the pivot
partition p []      = ([], [])
partition p (x:xs)
 | p >= x           = (x:smaller, larger)
 | otherwise        = (smaller, x:larger)
 where
  (smaller, larger) = partition p xs

quickSort :: Ord a => [a] -> [a] -- recursively partition a list until sorted (you stop when you partition the trival case)
quickSort []     = []
quickSort (x:xs) = quickSort (smaller) ++ [x] ++ quickSort (larger)
 where
  smaller        = fst (partition x xs)
  larger         = snd (partition x xs)

-- quick sort using list comprehension (less efficient in terms of memory)
qSort :: Ord a => [a] -> [a]
qSort [] = [] 
qSort (x:xs) = qSort (smaller) ++ [x] ++ qSort (larger)
 where
  smaller = [s | s <- xs, s <= x]
  larger  = [l | l <- xs, l > x]




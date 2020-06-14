{-
Name: Russell Rivera
File: MaximalSubArray.hs
Desc: Find the maximal contiguous subset of an array (DP)
-}

--TODO check algorithm correctness

module MaximalSubArray where

-- given an array of ints return the sum of the largest contiguous subarray 
maxSubArray :: [Int] -> Int
maxSubArray [] = 0
maxSubArray (x:xs) = maxSubArrayHelper (x:xs) x

-- given an array of integers, current maximum
maxSubArrayHelper :: [Int] -> Int -> Int
maxSubArrayHelper [] curMax = curMax
maxSubArrayHelper (x:xs) curMax = maxSubArrayHelper xs newMax
    where newMax = max x (curMax + x)

-- learn some basic IO for this
buildIOArr :: IO [Int]
buildIOArr = do
    input = getLine
    

main :: IO ()
main = do
    input <- getLine
    

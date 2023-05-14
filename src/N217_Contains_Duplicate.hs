module N217_Contains_Duplicate where

{-
https://leetcode.com/problems/contains-duplicate/

Given an integer array nums, return true if any value appears at least twice in the array, and
return false if every element is distinct.

Example 1:

Input: nums = [1,2,3,1]
Output: true

Example 2:

Input: nums = [1,2,3,4]
Output: false

Example 3:

Input: nums = [1,1,1,3,3,4,3,2,4,2]
Output: true
-}

import Lib.Utils

import Data.IntSet qualified as IntSet
import Data.List

-- >>> bruteForce [1,2,3,1]
-- True
-- >>> bruteForce [1,2,3,4]
-- False
-- >>> bruteForce [1,1,1,3,3,4,3,2,4,2]
-- True
bruteForce :: [Int] -> Bool
bruteForce []     = False
bruteForce (x:xs) = x `elem` xs || bruteForce xs

bruteForce' :: [Int] -> Bool
bruteForce' xs = not $ length (nub xs) == length xs

usingIntSet :: [Int] -> Bool
usingIntSet xs = not $ IntSet.size (IntSet.fromList xs) == length xs

usingIntSetInAcc :: [Int] -> Bool
usingIntSetInAcc = go IntSet.empty where
    go _   []     = False
    go acc (x:xs) = x `IntSet.member` acc || go (IntSet.insert x acc) xs

usingIntSetInAcc' :: [Int] -> Bool
usingIntSetInAcc' xs = foldr step (\_ -> False) xs IntSet.empty where
    step x rec acc = x `IntSet.member` acc || rec (IntSet.insert x acc)

-- >>> correctness
-- +++ OK, passed 1000 tests.
correctness :: IO ()
correctness = quickCheck $ \xs ->
    allEqual
        [ bruteForce xs
        , bruteForce xs
        , usingIntSet xs
        , usingIntSetInAcc xs
        , usingIntSetInAcc' xs
        ]

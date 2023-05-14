module N1_Two_Sum where

{-
https://leetcode.com/problems/two-sum/

Given an array of integers nums and an integer target, return indices of the two numbers such that they add up to target.

You may assume that each input would have exactly one solution, and you may not use the same element twice.

You can return the answer in any order.

Example 1:

Input: nums = [2,7,11,15], target = 9
Output: [0,1]
Explanation: Because nums[0] + nums[1] == 9, we return [0, 1].

Example 2:

Input: nums = [3,2,4], target = 6
Output: [1,2]

Example 3:

Input: nums = [3,3], target = 6
Output: [0,1]

Constraints:
    2 <= nums.length <= 10^4
    -10^9 <= nums[i] <= 10^9
    -10^9 <= target <= 10^9
    Only one valid answer exists.
-}

import Control.Applicative
import Data.HashMap.Strict qualified as HashMap
import Data.List
import Data.Maybe

-- >>> bruteForce 9 [2,7,11,15]
-- (0,1)
-- >>> bruteForce 6 [3,2,4]
-- (1,2)
-- >>> bruteForce 6 [3,3]
-- (0,1)
-- >>> bruteForce 0 [1,2,-5,3,4,5]
-- (2,5)
bruteForce :: Int -> [Int] -> (Int, Int)
bruteForce k = fromJust . go 0 where
    go !_    []     = Nothing
    go !curr (x:xs) = here <|> elsewhere where
        here      = do
            delta <- findIndex (\y -> x + y == k) xs
            pure (curr, curr + delta + 1)
        elsewhere = go (curr + 1) xs

-- >>> usingHashMap 9 [2,7,11,15]
-- (0,1)
-- >>> usingHashMap 6 [3,2,4]
-- (1,2)
-- >>> usingHashMap 6 [3,3]
-- (0,1)
-- >>> usingHashMap 0 [1,2,-5,3,4,5]
-- (2,5)
usingHashMap :: Int -> [Int] -> (Int, Int)
usingHashMap k = fromJust . go HashMap.empty 0 where
    go _   !_    []     = Nothing
    go acc !curr (x:xs) = case HashMap.lookup (k - x) acc of
        Nothing   ->
            -- May overwrite an already existing entry, but we don't care.
            let acc' = HashMap.insert x curr acc
            in go acc' (curr + 1) xs
        Just prev -> pure (prev, curr)

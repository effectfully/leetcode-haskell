module N347_Top_K_Frequent_Elements where

{-
Given an integer array nums and an integer k, return the k most frequent elements. You may return
the answer in any order.

Example 1:

Input: nums = [1,1,1,2,2,3], k = 2
Output: [1,2]

Example 2:

Input: nums = [1], k = 1
Output: [1]

Constraints:

    1 <= nums.length <= 105
    -104 <= nums[i] <= 104
    k is in the range [1, the number of unique elements in the array].
    It is guaranteed that the answer is unique.

Follow up: Your algorithm's time complexity must be better than O(n log n), where n is the array's
size.
-}

import Data.IntMap (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List
import Data.Ord

-- >>> nlogn 2 [1,1,1,2,2,3]
-- [1,2]
-- >>> nlogn 1 [1]
-- [1]
-- >>> nlogn 3 [2,6,1,3,2,6,1,2]
-- [2,1,6]
nlogn :: Int -> [Int] -> [Int]
nlogn k
    = take k
    . map (head . snd)
    . sortBy (flip $ comparing fst)
    . map (\xs -> (length xs, xs))
    . group
    . sort

toFrequencies :: [Int] -> IntMap Int
toFrequencies = IntMap.fromListWith (+) . map (\i -> (i, 1))

revertIntMap :: IntMap Int -> IntMap [Int]
revertIntMap = IntMap.fromListWith (++) . map (\(key, val) -> (val, [key])) . IntMap.toList

-- >>> linear 2 [1,1,1,2,2,3]
-- [1,2]
-- >>> linear 1 [1]
-- [1]
-- >>> linear 3 [2,6,1,3,2,6,1,2]
-- [2,6,1]
linear :: Int -> [Int] -> [Int]
linear k = take k . concatMap snd . IntMap.toDescList . revertIntMap . toFrequencies

module N128_Longest_Consecutive_Sequence where

{-
Given an unsorted array of integers nums, return the length of the longest consecutive elements
sequence.

You must write an algorithm that runs in O(n) time.

Example 1:

Input: nums = [100,4,200,1,3,2]
Output: 4
Explanation: The longest consecutive elements sequence is [1, 2, 3, 4]. Therefore its length is 4.

Example 2:

Input: nums = [0,3,7,2,5,8,4,6,0,1]
Output: 9

Constraints:

    0 <= nums.length <= 10^5
    -10^9 <= nums[i] <= 10^9
-}

import Lib.Utils

import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.List

-- >>> bruteForce [4,3,7,5,8,2,0,6,1]
-- 9
-- >>> bruteForce [100,4,200,1,3,2]
-- 4
bruteForce :: [Int] -> Int
bruteForce = maximum . map length . go [] . sort where
    go acc     []     = [acc]
    go []      (x:xs) = go [x] xs
    go (a:acc) (x:xs)
        | a == x     = go (a:acc) xs
        | a + 1 == x = go (x:a:acc) xs
        | otherwise  = (a:acc) : go [x] xs

walk :: (Int -> Int) -> Int -> HashSet Int -> ([Int], HashSet Int)
walk applyDelta = go where
    go key set
        | key `HashSet.member` set =
            let (consecs, set') = go (applyDelta key) $ HashSet.delete key set
            in (key : consecs, set')
        | otherwise = ([], set)

biwalk :: Int -> HashSet Int -> ([Int], HashSet Int)
biwalk key set
    | key `HashSet.member` set =
        let set' = HashSet.delete key set
            (consecsLeft, setMinusLeft)   = walk pred (pred key) set'
            (consecsRight, setMinusRight) = walk succ (succ key) setMinusLeft
        in (key : consecsLeft ++ consecsRight, setMinusRight)
    | otherwise = ([], set)

-- >>> consecutiveChunks [4,3,7,5,8,2,0,6,1]
-- [[4,3,2,1,0,5,6,7,8]]
-- >>> consecutiveChunks [100,4,200,1,3,2]
-- [[100],[4,3,2,1],[200]]
consecutiveChunks :: [Int] -> [[Int]]
-- Let's pretend 'HashMap.fromList' is linear.
consecutiveChunks nums = go (HashSet.fromList nums) nums where
    go _   []           = []
    go set (key : keys) =
        let (consecs, set') = biwalk key set
        in [consecs | not $ null consecs] ++ go set' keys

-- >>> usingHashSet [4,3,7,5,8,2,0,6,1]
-- 9
-- >>> usingHashSet [100,4,200,1,3,2]
-- 4
usingHashSet :: [Int] -> Int
usingHashSet = maximum . map length . consecutiveChunks

-- >>> correctness
-- +++ OK, passed 1000 tests.
correctness :: IO ()
correctness = quickCheck $ \x0 xs1 -> do
    let xs = x0 : xs1
    bruteForce xs === usingHashSet xs

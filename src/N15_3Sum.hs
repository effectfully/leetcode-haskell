-- I fucking hate this problem.
module N15_3Sum where

{-
Given an integer array nums, return all the triplets [nums[i], nums[j], nums[k]] such that i != j,
i != k, and j != k, and nums[i] + nums[j] + nums[k] == 0.

Notice that the solution set must not contain duplicate triplets.

Example 1:

Input: nums = [-1,0,1,2,-1,-4]
Output: [[-1,-1,2],[-1,0,1]]
Explanation:
nums[0] + nums[1] + nums[2] = (-1) + 0 + 1 = 0.
nums[1] + nums[2] + nums[4] = 0 + 1 + (-1) = 0.
nums[0] + nums[3] + nums[4] = (-1) + 2 + (-1) = 0.
The distinct triplets are [-1,0,1] and [-1,-1,2].
Notice that the order of the output and the order of the triplets does not matter.

Example 2:

Input: nums = [0,1,1]
Output: []
Explanation: The only possible triplet does not sum up to 0.

Example 3:

Input: nums = [0,0,0]
Output: [[0,0,0]]
Explanation: The only possible triplet sums up to 0.

Constraints:

    3 <= nums.length <= 3000
    -10^5 <= nums[i] <= 10^5
-}

import Lib.Utils

import Data.List
import Data.Vector (Vector)
import Data.Vector qualified as Vector

-- >>> bruteForceN 3 0 [-1,0,1,2,-1,-4]
-- [[-1,0,1],[-1,-1,2]]
-- >>> bruteForceN 3 0 [0,1,1]
-- []
-- >>> bruteForceN 3 0 [0,0,0]
-- [[0,0,0]]
-- >>> bruteForceN 3 0 [-1,-1,0,0,1,1]
-- [[-1,0,1]]
-- >>> bruteForceN 3 0 [-4,-4,-2,-1,-1,0,1,2,2,3,4,5,5,6,8,9]
-- [[-4,-4,8],[-4,-2,6],[-4,-1,5],[-4,0,4],[-4,1,3],[-4,2,2],[-2,-1,3],[-2,0,2],[-1,-1,2],[-1,0,1]]
bruteForceN :: Int -> Int -> [Int] -> [[Int]]
bruteForceN toTake0 target0 xs0 = nub . map sort $ go toTake0 target0 xs0 where
    go 0      target _      = [[] | target == 0]
    go _      _      []     = []
    go toTake target (x:xs) = withX ++ withoutX where
        withX    = map (x :) $ bruteForceN (toTake - 1) (target - x) xs
        withoutX = bruteForceN toTake target xs

-- >>> twoSumOrderedStarting 1 1 $ Vector.fromList [-1,-1,0,0,1,1]
-- [[0,1]]
-- >>> twoSumOrderedStarting 1 4 $ Vector.fromList [-4,-4,-2,-1,-1,0,1,2,2,3,4,5,5,6,8,9]
-- [[-4,8],[-2,6],[-1,5],[0,4],[1,3],[2,2]]
twoSumOrderedStarting :: Int -> Int -> Vector Int -> [[Int]]
twoSumOrderedStarting start target vec = go start (Vector.length vec - 1) Nothing where
    go i j mayPrev
        | i >= j                     = []
        | maybe False (== x) mayPrev = moveLeftToRight mayPrev
        | otherwise                  =
            case (x + y) `compare` target of
                EQ -> [x, y] : moveBoth (Just x)
                LT -> moveLeftToRight Nothing
                GT -> moveRightToLeft Nothing
        where
            x = vec Vector.! i
            y = vec Vector.! j
            moveLeftToRight = go (i + 1) j
            moveRightToLeft = go i       (j - 1)
            moveBoth        = go (i + 1) (j - 1)

-- >>> usingTwoSumOrdered [-1,-1,0,0,1,1]
-- [[-1,0,1]]
-- >>> usingTwoSumOrdered [-4,-4,-2,-1,-1,0,1,2,2,3,4,5,5,6,8,9]
-- [[-4,-4,8],[-4,-2,6],[-4,-1,5],[-4,0,4],[-4,1,3],[-4,2,2],[-2,-1,3],[-2,0,2],[-1,-1,2],[-1,0,1]]
usingTwoSumOrdered :: [Int] -> [[Int]]
usingTwoSumOrdered xsUnord = go Nothing 0 where
    xsVec = Vector.fromList $ sort xsUnord

    go mayPrev currIx
        | currIx < Vector.length xsVec =
            let curr = xsVec Vector.! currIx
            in concat
                [ if maybe False (== curr) mayPrev
                    then []
                    else map (curr :) $ twoSumOrderedStarting (currIx + 1) (- curr) xsVec
                , go (Just curr) (currIx + 1)
                ]
        | otherwise = []

-- >>> correctness
-- +++ OK, passed 100 tests:
-- 30% 0
--  7% 2
--  4% 1
--  3% 11
--  3% 3
--  3% 35
--  3% 4
--  2% 10
--  2% 118
--  2% 12
--  2% 14
--  2% 17
--  2% 26
--  2% 29
--  2% 44
--  2% 5
--  2% 6
--  1% 114
--  1% 127
--  1% 13
--  1% 137
--  1% 142
--  1% 15
--  1% 16
--  1% 162
--  1% 20
--  1% 208
--  1% 21
--  1% 22
--  1% 23
--  1% 24
--  1% 25
--  1% 27
--  1% 37
--  1% 42
--  1% 46
--  1% 56
--  1% 61
--  1% 64
--  1% 77
--  1% 8
--  1% 83
--  1% 87
--  1% 9
correctness :: IO ()
correctness = quickCheck . withMaxSuccess 100 $ \xs -> do
    let answer = sort $ bruteForceN 3 0 xs
    collect (length answer) $ answer === usingTwoSumOrdered xs

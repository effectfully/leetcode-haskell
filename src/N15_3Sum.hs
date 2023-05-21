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
-- 27% 0
--  6% 1
--  6% 2
--  5% 14
--  3% 10
--  3% 3
--  2% 12
--  2% 13
--  2% 16
--  2% 28
--  2% 34
--  2% 42
--  2% 6
--  2% 89
--  2% 9
--  1% 100
--  1% 109
--  1% 11
--  1% 15
--  1% 162
--  1% 20
--  1% 21
--  1% 210
--  1% 23
--  1% 26
--  1% 30
--  1% 31
--  1% 32
--  1% 36
--  1% 37
--  1% 38
--  1% 41
--  1% 43
--  1% 5
--  1% 52
--  1% 53
--  1% 54
--  1% 57
--  1% 59
--  1% 60
--  1% 63
--  1% 64
--  1% 67
--  1% 7
--  1% 79
--  1% 8
--  1% 94
correctness :: IO ()
correctness = quickCheck . withMaxSuccess 100 $ \xs -> do
    let answer = sort $ bruteForceN 3 0 xs
    classify True (show $ length answer) $ usingTwoSumOrdered xs === answer

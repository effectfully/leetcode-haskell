module N167_Two_Sum_II_Input_Array_Is_Sorted where

{-
Given a 1-indexed array of integers numbers that is already sorted in non-decreasing order, find two
numbers such that they add up to a specific target number. Let these two numbers be numbers[index1]
and numbers[index2] where 1 <= index1 < index2 <= numbers.length.

Return the indices of the two numbers, index1 and index2, added by one as an integer array [index1,
index2] of length 2.

The tests are generated such that there is exactly one solution. You may not use the same element
twice.

Your solution must use only constant extra space.

Example 1:

Input: numbers = [2,7,11,15], target = 9
Output: [1,2]
Explanation: The sum of 2 and 7 is 9. Therefore, index1 = 1, index2 = 2. We return [1, 2].

Example 2:

Input: numbers = [2,3,4], target = 6
Output: [1,3]
Explanation: The sum of 2 and 4 is 6. Therefore index1 = 1, index2 = 3. We return [1, 3].

Example 3:

Input: numbers = [-1,0], target = -1
Output: [1,2]
Explanation: The sum of -1 and 0 is -1. Therefore index1 = 1, index2 = 2. We return [1, 2].

Constraints:

    2 <= numbers.length <= 3 * 10^4
    -1000 <= numbers[i] <= 1000
    numbers is sorted in non-decreasing order.
    -1000 <= target <= 1000
    The tests are generated such that there is exactly one solution.
-}

import Lib.Utils
import N1_Two_Sum qualified as N1_Two_Sum

import Data.Bifunctor
import Data.List
import Data.Vector qualified as Vector

-- >>> bruteForce 9 [2,7,11,15]
-- (1,2)
-- >>> bruteForce 6 [2,3,4]
-- (1,3)
-- >>> bruteForce (-1) [-1, 0]
-- (1,2)
-- >>> bruteForce 4 [0, 1, 2, 2, 5]
-- (3,4)
bruteForce :: Int -> [Int] -> (Int, Int)
bruteForce target = bimap succ succ . N1_Two_Sum.bruteForce target

-- >>> biwalk 9 [2,7,11,15]
-- (1,2)
-- >>> biwalk 6 [2,3,4]
-- (1,3)
-- >>> biwalk (-1) [-1, 0]
-- (1,2)
-- >>> biwalk 4 [0, 1, 2, 2, 5]
-- (3,4)
biwalk :: Int -> [Int] -> (Int, Int)
biwalk target numbers = bimap succ succ $ go 0 (Vector.length numbersVec - 1) where
    numbersVec = Vector.fromList numbers

    go :: Int -> Int -> (Int, Int)
    go i j
        | i >= j    = error "No solution found"
        | otherwise =
            case (numbersVec Vector.! i + numbersVec Vector.! j) `compare` target of
                EQ -> (i, j)
                LT -> go (i + 1) j
                GT -> go i (j - 1)

-- >>> correctness
-- +++ OK, passed 1000 tests; 17 discarded.
correctness :: IO ()
correctness = quickCheck $ \x0 x1 xs2 -> x0 /= x1 ==> do
    let xs = map head . group . sort $ x0 : x1 : xs2
        target = x0 + x1
    bruteForce target xs === biwalk target xs

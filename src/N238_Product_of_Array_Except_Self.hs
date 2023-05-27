module N238_Product_of_Array_Except_Self where

{-
Given an integer array nums, return an array answer such that answer[i] is equal to the product of all the elements of nums except nums[i].

The product of any prefix or suffix of nums is guaranteed to fit in a 32-bit integer.

You must write an algorithm that runs in O(n) time and without using the division operation.

Example 1:

Input: nums = [1,2,3,4]
Output: [24,12,8,6]

Example 2:

Input: nums = [-1,1,0,-3,3]
Output: [0,0,9,0,0]

Constraints:

    2 <= nums.length <= 105
    -30 <= nums[i] <= 30
    The product of any prefix or suffix of nums is guaranteed to fit in a 32-bit integer.
-}

import Lib.Utils

-- >>> quadratic [1,2,3,4]
-- [24,12,8,6]
-- >>> quadratic [-1,1,0,-3,3]
-- [0,0,9,0,0]
-- >>> quadratic [-5,2,4,0,1,2,10,-7,0]
-- [0,0,0,0,0,0,0,0,0]
-- [820800,-615600,-273600,-2462400,1231200,-492480,-129600,410400,-492480,-615600,-2462400]
-- >>> quadratic [-3,4,9,1,-2,5,19,-6,5,4,1]
quadratic :: [Int] -> [Int]
quadratic = go 1 where
    go _   []     = []
    go acc (n:ns) = product (acc : ns) : go (acc * n) ns

-- >>> linear [1,2,3,4]
-- [24,12,8,6]
-- >>> linear [-1,1,0,-3,3]
-- [0,0,9,0,0]
-- >>> linear [-5,2,4,0,1,2,10,-7,0]
-- [0,0,0,0,0,0,0,0,0]
-- >>> linear [-3,4,9,1,-2,5,19,-6,5,4,1]
-- [820800,-615600,-273600,-2462400,1231200,-492480,-129600,410400,-492480,-615600,-2462400]
linear :: [Int] -> [Int]
linear ns0 = go 1 cache ns0 where
    cache = tail $ scanr1 (*) ns0

    go acc []     _      = [acc]
    go _   (_:_)  []     = error "Panic: the impossible happened"
    go acc (c:cs) (n:ns) = acc * c : go (acc * n) cs ns

-- >>> correctness
-- +++ OK, passed 1000 tests.
correctness :: IO ()
correctness = quickCheck $ \n0 n1 ns2 -> do
    let ns = n0 : n1 : ns2
    quadratic ns === linear ns

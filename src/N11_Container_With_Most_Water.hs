module N11_Container_With_Most_Water where

{-
You are given an integer array height of length n. There are n vertical lines drawn such that the
two endpoints of the ith line are (i, 0) and (i, height[i]).

Find two lines that together with the x-axis form a container, such that the container contains the
most water.

Return the maximum amount of water a container can store.

Notice that you may not slant the container.

Example 1:

Input: height = [1,8,6,2,5,4,8,3,7]
Output: 49
Explanation: The above vertical lines are represented by array [1,8,6,2,5,4,8,3,7]. In this case,
the max area of water (blue section) the container can contain is 49.

Example 2:

Input: height = [1,1]
Output: 1

Constraints:

    n == height.length
    2 <= n <= 105
    0 <= height[i] <= 104
-}

import Lib.Utils

import Data.Vector qualified as Vector

-- >>> quadratic [1,8,6,2,5,4,8,3,7]
-- 49
-- >>> quadratic [1,1]
-- 1
quadratic :: [Int] -> Int
quadratic []                  = 0
quadratic (height1 : heights) =
    max
        (maximum $ 0 : zipWith (\dist height2 -> min height1 height2 * dist) [1..] heights)
        (quadratic heights)

-- >>> linear [1,8,6,2,5,4,8,3,7]
-- 49
-- >>> linear [1,1]
-- 1
linear :: [Int] -> Int
linear height = go maxSoFar0 l0 r0 where
    l0        = 0
    r0        = Vector.length heightVec - 1
    maxSoFar0 = toArea l0 r0

    toArea l r = (r - l) * min (heightVec Vector.! l) (heightVec Vector.! r)

    heightVec = Vector.fromList height

    go maxSoFar l r
        | l >= r    = maxSoFar
        | otherwise = go (max maxSoFar $ toArea l' r') l' r'
        where
            (l', r')
                | heightVec Vector.! l < heightVec Vector.! r = (l + 1, r)
                | otherwise                                   = (l, r - 1)

-- >>> correctness
-- +++ OK, passed 1000 tests.
correctness :: IO ()
correctness = quickCheck $ \x0 x1 xs2 -> do
    let xs = x0 : x1 : xs2
    quadratic xs === linear xs

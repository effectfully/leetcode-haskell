module N121_Best_Time_to_Buy_and_Sell_Stock where

{-
You are given an array prices where prices[i] is the price of a given stock on the ith day.

You want to maximize your profit by choosing a single day to buy one stock and choosing a different day in the future to sell that stock.

Return the maximum profit you can achieve from this transaction. If you cannot achieve any profit, return 0.

Example 1:

Input: prices = [7,1,5,3,6,4]
Output: 5
Explanation: Buy on day 2 (price = 1) and sell on day 5 (price = 6), profit = 6-1 = 5.
Note that buying on day 2 and selling on day 1 is not allowed because you must buy before you sell.

Example 2:

Input: prices = [7,6,4,3,1]
Output: 0
Explanation: In this case, no transactions are done and the max profit = 0.

Constraints:

    1 <= prices.length <= 10^5
    0 <= prices[i] <= 10^4
-}

import Lib.Utils

-- >>> quadratic [7,1,5,3,6,4]
-- 5
-- >>> quadratic [7,6,4,3,1]
-- 0
quadratic :: [Int] -> Int
quadratic []     = 0
quadratic (x:xs) = max (maximum (0:xs) - x) (quadratic xs)

-- >>> linear [7,1,5,3,6,4]
-- 5
-- >>> linear [7,6,4,3,1]
-- 0
linear :: [Int] -> Int
linear []         = 0
linear (x0 : xs0) = go 0 x0 xs0 where
    go maxDiff _        []     = maxDiff
    go maxDiff minPrice (x:xs) = go maxDiff' minPrice' xs where
        maxDiff'  = max maxDiff $ x - minPrice
        minPrice' = min minPrice x

-- >>> correctness
-- +++ OK, passed 1000 tests.
correctness :: IO ()
correctness = quickCheck $ \xsPre -> do
    let xs = map abs xsPre
    quadratic xs === linear xs

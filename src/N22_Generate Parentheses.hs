module N22_Generate_Parentheses where

{-
Given n pairs of parentheses, write a function to generate all combinations of well-formed parentheses.

Example 1:

Input: n = 3
Output: ["((()))","(()())","(())()","()(())","()()()"]

Example 2:

Input: n = 1
Output: ["()"]

Constraints:

    1 <= n <= 8
-}

-- >>> genValid 0
-- [""]
-- >>> genValid 1
-- ["()"]
-- >>> genValid 2
-- ["()()","(())"]
-- >>> genValid 3
-- ["()()()","()(())","(())()","(()())","((()))"]
-- >>> genValid 4
-- ["()()()()","()()(())","()(())()","()(()())","()((()))","(())()()","(())(())","(()())()","((()))()","(()()())","(()(()))","((())())","((()()))","(((())))"]
genValid :: Int -> [String]
genValid 0 = [""]
genValid n = do
    i <- [0 .. n - 1]
    l <- genValid i
    r <- genValid $ n - i - 1
    ["(" ++ l ++ ")" ++ r]

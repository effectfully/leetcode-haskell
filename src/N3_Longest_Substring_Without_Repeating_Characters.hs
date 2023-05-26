{-# LANGUAGE LambdaCase #-}
module N3_Longest_Substring_Without_Repeating_Characters where

{-
Given a string s, find the length of the longest substring without repeating characters.

Example 1:

Input: s = "abcabcbb"
Output: 3
Explanation: The answer is "abc", with the length of 3.

Example 2:

Input: s = "bbbbb"
Output: 1
Explanation: The answer is "b", with the length of 1.

Example 3:

Input: s = "pwwkew"
Output: 3
Explanation: The answer is "wke", with the length of 3.
Notice that the answer must be a substring, "pwke" is a subsequence and not a substring.

Constraints:

    0 <= s.length <= 5 * 10^4
    s consists of English letters, digits, symbols and spaces.
-}

import Lib.Utils

import Data.HashMap.Strict qualified as HashMap

-- >>> quadratic "abcabcbb"
-- 3
-- >>> quadratic "bbbbb"
-- 1
-- >>> quadratic "pwwkew"
-- 3
-- >>> quadratic ""
-- 0
-- >>> quadratic "a"
-- 1
-- >>> quadratic "aba"
-- 2
-- >>> quadratic "abcbde"
-- 4
quadratic :: String -> Int
quadratic = go 0 "" where
    go maxLen _             ""         = maxLen
    go maxLen lastSubstring (ch : str) = go maxLen' lastSubstring' str where
        lastSubstring' = ch : takeWhile (/= ch) lastSubstring
        maxLen' = max maxLen $ length lastSubstring'

-- >>> linear "abcabcbb"
-- 3
-- >>> linear "bbbbb"
-- 1
-- >>> linear "pwwkew"
-- 3
-- >>> linear ""
-- 0
-- >>> linear "a"
-- 1
-- >>> linear "aba"
-- 2
-- >>> linear "abb"
-- 2
-- >>> linear "abcbde"
-- 4
linear :: String -> Int
linear = go 0 0 0 HashMap.empty where
    go maxLen start currIndex indices =
        let maxLen' = max maxLen $ currIndex - start
        in \case
            ""       -> maxLen'
            ch : str ->
                let currIndex' = currIndex + 1
                    indices'   = HashMap.insert ch currIndex indices
                in case HashMap.lookup ch indices of
                    Just chIndex | chIndex >= start ->
                        go maxLen' (chIndex + 1) currIndex' indices' str
                    _ -> go maxLen start currIndex' indices' str

-- >>> correctness
-- +++ OK, passed 1000 tests.
correctness :: IO ()
correctness = quickCheck $ \(ASCIIString str) -> quadratic str === linear str

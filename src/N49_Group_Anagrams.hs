module N49_Group_Anagrams where

{-
Given an array of strings strs, group the anagrams together. You can return the answer in any order.

An Anagram is a word or phrase formed by rearranging the letters of a different word or phrase,
typically using all the original letters exactly once.

Example 1:

Input: strs = ["eat","tea","tan","ate","nat","bat"]
Output: [["bat"],["nat","tan"],["ate","eat","tea"]]

Example 2:

Input: strs = [""]
Output: [[""]]

Example 3:

Input: strs = ["a"]
Output: [["a"]]

Constraints:

    1 <= strs.length <= 104
    0 <= strs[i].length <= 100
    strs[i] consists of lowercase English letters.
-}

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap

toAnagram :: String -> HashMap Char Int
toAnagram = HashMap.fromListWith (+) . map (\c -> (c, 1))

-- >>> linear ["eat","tea","tan","ate","nat","bat"]
-- [["bat"],["ate","tea","eat"],["nat","tan"]]
-- >>> linear [""]
-- [[""]]
-- >>> linear ["a"]
-- [["a"]]
linear :: [String] -> [[String]]
linear = HashMap.elems . HashMap.fromListWith (++) . map (\str -> (toAnagram str, [str]))

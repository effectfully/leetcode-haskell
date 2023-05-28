{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}
module N424_Longest_Repeating_Character_Replacement where

{-
You are given a string s and an integer k. You can choose any character of the string and change it to any other uppercase English character. You can perform this operation at most k times.

Return the length of the longest substring containing the same letter you can get after performing the above operations.

Example 1:

Input: s = "ABAB", k = 2
Output: 4
Explanation: Replace the two 'A's with two 'B's or vice versa.

Example 2:

Input: s = "AABABBA", k = 1
Output: 4
Explanation: Replace the one 'A' in the middle with 'B' and form "AABBBBA".
The substring "BBBB" has the longest repeating letters, which is 4.
There may exists other ways to achive this answer too.

Constraints:

    1 <= s.length <= 105
    s consists of only uppercase English letters.
    0 <= k <= s.length
-}

import Lib.Utils

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.List
import Data.Maybe

-- >>> sublistsDesc ""
-- [""]
-- >>> sublistsDesc "abcde"
-- ["abcde","bcde","abcd","cde","bcd","abc","de","cd","bc","ab","e","d","c","b","a",""]
sublistsDesc :: [a] -> [[a]]
sublistsDesc = reverse . ([] :) . concat . transpose . go where
    go xs = tail (inits xs) : case xs of
        []    -> []
        _:xs' -> go xs'

isValid :: Int -> String -> Bool
isValid k str =
    case reverse . sort . HashMap.elems . HashMap.fromListWith (+) $ map (, 1) str of
        []         -> True
        _ : others -> sum others <= k

-- >>> bruteForce 2 "ABAB"
-- 4
-- >>> bruteForce 1 "AABABBA"
-- 4
-- >>> bruteForce 2 "AABBBAAAAA"
-- 7
-- >>> bruteForce 1 "ABBAB"
-- 4
bruteForce :: Int -> String -> Int
bruteForce k = length . fromMaybe "" . find (isValid k) . sublistsDesc

makeValid :: Int -> String -> HashMap Char Int -> (Int, String, HashMap Char Int)
makeValid k = go where
    go substringAndRest occurs = case reverse . sort $ HashMap.elems occurs of
        []                  -> (0, substringAndRest, occurs)
        dominating : others ->
            let othersSum = sum others
            in if othersSum <= k
                then (dominating + othersSum, substringAndRest, occurs)
                else case substringAndRest of
                         ""                     -> (0, substringAndRest, occurs)
                         ch : substringAndRest' ->
                             go substringAndRest' $ HashMap.adjust pred ch occurs

-- >>> linear 2 "ABAB"
-- 4
-- >>> linear 1 "AABABBA"
-- 4
-- >>> linear 2 "AABBBAAAAA"
-- 7
-- >>> linear 1 "ABBAB"
-- 4
linear :: Int -> String -> Int
linear k str0 = go 0 str0 HashMap.empty str0 where
    go :: Int -> String -> HashMap Char Int -> String -> Int
    go maxSoFar substringAndRest occurs = \case
        ""       -> maxSoFar
        ch : str ->
            let (maxCandidate, substringAndRest', occurs') =
                    makeValid k substringAndRest $ HashMap.insertWith (+) ch 1 occurs
            in go (max maxSoFar maxCandidate) substringAndRest' occurs' str

-- >>> correctness
-- +++ OK, passed 500 tests.
correctness :: IO ()
correctness = quickCheck . withMaxSuccess 500 $ \(Small k) (ASCIIString str) ->
    bruteForce k str === linear k str

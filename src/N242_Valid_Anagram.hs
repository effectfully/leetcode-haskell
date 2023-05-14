module N242_Valid_Anagram where

{-
https://leetcode.com/problems/valid-anagram/

Given two strings s and t, return true if t is an anagram of s, and false otherwise.

An Anagram is a word or phrase formed by rearranging the letters of a different word or phrase, typically using all the original letters exactly once.

Example 1:

Input: s = "anagram", t = "nagaram"
Output: true

Example 2:

Input: s = "rat", t = "car"
Output: false
-}

import Lib.Utils

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.List

-- >>> bruteForce "anagram" "nagaram"
-- True
-- >>> bruteForce "rat" "car"
-- False
bruteForce :: String -> String -> Bool
bruteForce str1 str2 = sort str1 == sort str2

toAnagram :: String -> HashMap Char Int
toAnagram = HashMap.fromListWith (+) . map (\c -> (c, 1))

usingHashMap :: String -> String -> Bool
usingHashMap str1 str2 = toAnagram str1 == toAnagram str2

-- >>> correctness
-- +++ OK, passed 1000 tests.
-- +++ OK, passed 1000 tests.
-- +++ OK, passed 1000 tests.
correctness :: IO ()
correctness = do
    quickCheck $ \(PrintableString str1) ->
        forAll (shuffle str1) $ \str2 ->
            allEqual
                [ True
                , bruteForce str1 str2
                , usingHashMap str1 str2
                ]
    quickCheck $ \(NonEmpty str1) ->
        forAll (shuffle $ take 1 str1 ++ str1) $ \str2 ->
            allEqual
                [ False
                , bruteForce str1 str2
                , usingHashMap str1 str2
                ]
    quickCheck $ \(ASCIIString str1) (ASCIIString str2) ->
        bruteForce str1 str2 == usingHashMap str1 str2

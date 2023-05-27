module N125_Valid_Palindrome where

{-
A phrase is a palindrome if, after converting all uppercase letters into lowercase letters and removing all non-alphanumeric characters, it reads the same forward and backward. Alphanumeric characters include letters and numbers.

Given a string s, return true if it is a palindrome, or false otherwise.

Example 1:

Input: s = "A man, a plan, a canal: Panama"
Output: true
Explanation: "amanaplanacanalpanama" is a palindrome.

Example 2:

Input: s = "race a car"
Output: false
Explanation: "raceacar" is not a palindrome.

Example 3:

Input: s = " "
Output: true
Explanation: s is an empty string "" after removing non-alphanumeric characters.
Since an empty string reads the same forward and backward, it is a palindrome.

Constraints:

    1 <= s.length <= 2 * 10^5
    s consists only of printable ASCII characters.
-}

import Lib.Utils

import Data.Char
import Data.Vector qualified as Vector

-- >>> prepare "A man, a plan, a canal: Panama"
-- "amanaplanacanalpanama"
prepare :: String -> String
prepare = filter isAlphaNum . map toLower

-- >>> bruteForce "A man, a plan, a canal: Panama"
-- True
-- >>> bruteForce "race a car"
-- False
-- >>> bruteForce " "
-- True
bruteForce :: String -> Bool
bruteForce str = strPrep == reverse strPrep where
    strPrep = prepare str

-- >>> twoPointers "A man, a plan, a canal: Panama"
-- True
-- >>> twoPointers "race a car"
-- False
-- >>> twoPointers " "
-- True
twoPointers :: String -> Bool
twoPointers str = go 0 $ Vector.length vec - 1 where
    vec = Vector.fromList $ prepare str

    go i j
        | i >= j    = True
        | otherwise = vec Vector.! i == vec Vector.! j && go (i + 1) (j - 1)

-- >>> correctness
-- +++ OK, passed 1000 tests.
correctness :: IO ()
correctness = do
    quickCheck $ \(ASCIIChar middle) (ASCIIString strL) -> do
        let strEven = strL ++ reverse strL
            strOdd  = strL ++ [middle] ++ reverse strL
        conjoin
            [ bruteForce strL === twoPointers strL
            , allEqual
                [ True
                , bruteForce strEven
                , twoPointers strEven
                , bruteForce strOdd
                , twoPointers strOdd
                ]
            ]

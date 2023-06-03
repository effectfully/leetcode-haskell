module N1032_Stream_of_Characters where

{-
Design an algorithm that accepts a stream of characters and checks if a suffix of these characters
is a string of a given array of strings words.

For example, if words = ["abc", "xyz"] and the stream added the four characters (one by one) 'a',
'x', 'y', and 'z', your algorithm should detect that the suffix "xyz" of the characters "axyz"
matches "xyz" from words.

Implement the StreamChecker class:

    StreamChecker(String[] words) Initializes the object with the strings array words.
    boolean query(char letter) Accepts a new character from the stream and returns true if any
      non-empty suffix from the stream forms a word that is in words.

Example 1:

Input
["StreamChecker", "query", "query", "query", "query", "query", "query", "query", "query", "query", "query", "query", "query"]
[[["cd", "f", "kl"]], ["a"], ["b"], ["c"], ["d"], ["e"], ["f"], ["g"], ["h"], ["i"], ["j"], ["k"], ["l"]]
Output
[null, false, false, false, true, false, true, false, false, false, false, false, true]

Explanation
StreamChecker streamChecker = new StreamChecker(["cd", "f", "kl"]);
streamChecker.query("a"); // return False
streamChecker.query("b"); // return False
streamChecker.query("c"); // return False
streamChecker.query("d"); // return True, because 'cd' is in the wordlist
streamChecker.query("e"); // return False
streamChecker.query("f"); // return True, because 'f' is in the wordlist
streamChecker.query("g"); // return False
streamChecker.query("h"); // return False
streamChecker.query("i"); // return False
streamChecker.query("j"); // return False
streamChecker.query("k"); // return False
streamChecker.query("l"); // return True, because 'kl' is in the wordlist

Constraints:

    1 <= words.length <= 2000
    1 <= words[i].length <= 200
    words[i] consists of lowercase English letters.
    letter is a lowercase English letter.
    At most 4 * 10^4 calls will be made to query.
-}

import Lib.Utils

import Data.Bifunctor
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.List
import Data.Maybe

-- [False,False,False,True,False,True,False,False,False,False,False,True]
bruteForce :: [String] -> [Char] -> [Bool]
bruteForce wrds = map (any (`elem` wrds) . init . tails) . tail . inits

newtype Trie = Trie (HashMap Char (Bool, Trie))

emptyTrie :: Trie
emptyTrie = Trie HashMap.empty

stringToTrie :: String -> Trie
stringToTrie ""           = error "empty words aren't allowed"
stringToTrie (ch0 : str0) = go ch0 str0 where
    go ch1 ""          = Trie $ HashMap.singleton ch1 (True, emptyTrie)
    go ch1 (ch2 : str) = Trie $ HashMap.singleton ch1 (False, go ch2 str)

mergeTries :: Trie -> Trie -> Trie
mergeTries (Trie hashMapL) (Trie hashMapR) = Trie $ HashMap.unionWith merger hashMapL hashMapR where
    merger (endsWordL, trieL) (endsWordR, trieR) =
        (endsWordL || endsWordR, mergeTries trieL trieR)

buildTrie :: [String] -> Trie
-- TODO: use @foldt@.
buildTrie = foldl' mergeTries emptyTrie . map stringToTrie

matchTrie :: Char -> Trie -> (Bool, Maybe Trie)
matchTrie ch (Trie hashMap) = case HashMap.lookup ch hashMap of
    Nothing               -> (False, Nothing)
    Just (endsWord, trie) -> (endsWord, Just trie)

matchAnyTrie :: Char -> [Trie] -> (Bool, [Trie])
matchAnyTrie ch = bimap or catMaybes . unzip . map (matchTrie ch)

checkSuffixes :: Trie -> [Char] -> [Bool]
checkSuffixes trie0 = go [] where
    go :: [Trie] -> [Char] -> [Bool]
    go _             []         = []
    go triesMatching (ch : chs) = endsWord : go triesMatching' chs where
        (endsWord, triesMatching') = matchAnyTrie ch $ trie0 : triesMatching

-- >>> usingTrie ["cd", "f", "kl"] "abcdefghijkl"
-- [False,False,False,True,False,True,False,False,False,False,False,True]
usingTrie :: [String] -> [Char] -> [Bool]
usingTrie = checkSuffixes . buildTrie

-- >>> correctness 2
-- +++ OK, passed 500 tests:
-- 22.0% 0
-- 10.4% 1
--  9.8% 2
--  4.6% 3
--  4.2% 4
--  3.2% 5
-- >>> correctness 3
-- +++ OK, passed 500 tests:
-- 53.0% 0
-- 14.6% 1
--  5.4% 2
--  3.6% 4
--  3.2% 3
--  2.8% 5
-- >>> correctness 4
-- +++ OK, passed 500 tests:
-- 62.6% 0
--  8.2% 1
--  5.6% 2
--  3.4% 3
--  2.8% 4
--  1.4% 5
correctness :: Int -> IO ()
correctness shootForAtLeast = quickCheck . withMaxSuccess 500 $ \(NonEmpty prewrds) prestream -> do
    let cut wrd = take (max shootForAtLeast $ length wrd `rem` 10) wrd
        wrds    = map (map getUpperCaseLetter . cut . getNonEmpty) prewrds
        stream  = map getUpperCaseLetter prestream
        answer  = bruteForce wrds stream
        matches = length $ filter id answer
    classify (matches <= 5) (show matches) $ answer === usingTrie wrds stream

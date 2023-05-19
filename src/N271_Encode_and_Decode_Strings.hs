{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module N271_Encode_and_Decode_Strings where

{-
Design an algorithm to encode a list of strings to a string. The encoded string is then sent over
the network and is decoded back to the original list of strings.

Please implement encode and decode

Example1

Input: ["lint","code","love","you"]

Output: ["lint","code","love","you"]

Explanation:

One possible encode method is: "lint:;code:;love:;you"

Example2

Input: ["we", "say", ":", "yes"]

Output: ["we", "say", ":", "yes"]

Explanation:

One possible encode method is: "we:;say:;:::;yes"
-}

import Lib.Utils

import Data.Coerce

pattern Separator :: Char
pattern Separator = '|'

-- >>> encode ["lint","code","love","you"]
-- "4|lint4|code4|love3|you"
-- >>> encode ["we", "say", "|", "yes"]
-- "2|we3|say1||3|yes"
-- >>> encode ["", "we", "say", "", "yes", ""]
-- "0|2|we3|say0|3|yes0|"
encode :: [String] -> String
encode = concatMap $ \str -> show (length str) ++ "|" ++ str

-- >>> decode "4|lint4|code4|love3|you"
-- ["lint","code","love","you"]
-- >>> decode "2|we3|say1||3|yes"
-- ["we","say","|","yes"]
-- >>> decode "0|2|we3|say0|3|yes0|"
-- ["","we","say","","yes",""]
decode :: String -> [String]
decode "" = []
decode str
    | (len, Separator : rest) <- break (== Separator) str
    , (chunk, str') <- splitAt (read len) rest
    = chunk : decode str'
decode str = error $ "Panic: unhandled tail of the string: " ++ str

newtype Chunk = Chunk
    { unChunk :: String
    } deriving newtype (Show, Eq)

instance Arbitrary Chunk where
    arbitrary = fmap Chunk . listOf $ frequency
        [ (9, arbitraryASCIIChar)
        , (1, pure Separator)
        ]

    shrink = coerce (shrink :: ASCIIString -> [ASCIIString])

-- >>> correctness
-- +++ OK, passed 1000 tests.
correctness :: IO ()
correctness = quickCheck $ \(chunks :: [Chunk]) -> coerce (decode . encode) chunks === chunks

{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
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

-- >>> encode1 ["lint","code","love","you"]
-- "4|lint4|code4|love3|you"
-- >>> encode1 ["we", "say", "|", "yes"]
-- "2|we3|say1||3|yes"
-- >>> encode1 ["", "we", "say", "", "yes", ""]
-- "0|2|we3|say0|3|yes0|"
encode1 :: [String] -> String
encode1 = concatMap $ \str -> show (length str) ++ "|" ++ str

-- >>> decode1 "4|lint4|code4|love3|you"
-- ["lint","code","love","you"]
-- >>> decode1 "2|we3|say1||3|yes"
-- ["we","say","|","yes"]
-- >>> decode1 "0|2|we3|say0|3|yes0|"
-- ["","we","say","","yes",""]
decode1 :: String -> [String]
decode1 "" = []
decode1 str
    | (len, Separator : rest) <- break (== Separator) str
    , (chunk, str') <- splitAt (read len) rest
    = chunk : decode1 str'
decode1 str = error $ "Panic: unhandled tail of the string: " ++ str

-- Below is an encoding-decoding procedure capable of handling infinite streams.

pattern Escape :: Char
pattern Escape = '\''

keychars :: [Char]
keychars = [Escape, Separator]

quote :: Char -> String
quote c
    | c `elem` keychars = [Escape, c]
    | otherwise         = [c]

unquote :: Char -> Char
unquote c
    | c `elem` keychars = c
    | otherwise         = error $ "Panic: '" ++ [c] ++ "' cannot be unquoted"

-- >>> encode2 ["lint","code","love","you"]
-- "lint|code|love|you|"
-- >>> encode2 ["we", "say", "|", "yes"]
-- "we|say|'||yes|"
-- >>> encode2 ["", "we", "say", "", "yes", ""]
-- "|we|say||yes||"
-- >>> encode2 ["'|", "'''|w|e'", "say'", "''", "|", "'y'es|"]
-- "'''||'''''''|w'|e''|say''|''''|'||''y''es'||"
encode2 :: [String] -> String
encode2 = concatMap $ \str -> concatMap quote str ++ "|"

data DecodeState
    = DecodeRegular
    | DecodeEscaped

assembleChunks :: [Maybe Char] -> [String]
assembleChunks []              = []
assembleChunks (Nothing : mcs) = [] : assembleChunks mcs
assembleChunks (Just c  : mcs) =
    let (chunk, chunks) = case assembleChunks mcs of
            []               -> ([], [])
            chunk' : chunks' -> (chunk', chunks')
    in (c : chunk) : chunks

-- >>> decode2 "lint|code|love|you|"
-- ["lint","code","love","you"]
-- >>> decode2 "we|say|'||yes|"
-- ["we","say","|","yes"]
-- >>> decode2 "|we|say||yes||"
-- ["","we","say","","yes",""]
-- >>> decode2 "'''||'''''''|w'|e''|say''|''''|'||''y''es'||"
-- ["'|","'''|w|e'","say'","''","|","'y'es|"]
decode2 :: String -> [String]
decode2 = assembleChunks . go DecodeRegular where
    go DecodeEscaped = \case
        []      ->  error "Panic: escaped empty string"
        c : str -> Just (unquote c) : go DecodeRegular str
    go DecodeRegular = \case
        []              -> []
        c : str -> case c of
            Escape    -> go DecodeEscaped str
            Separator -> Nothing : go DecodeRegular str
            _         -> Just c  : go DecodeRegular str

newtype Chunk = Chunk
    { unChunk :: String
    } deriving newtype (Show, Eq)

instance Arbitrary Chunk where
    arbitrary = fmap Chunk . listOf $ frequency
        [ (9, arbitraryASCIIChar)
        , (1, pure Separator)
        , (1, pure Escape)
        ]

    shrink = coerce (shrink :: ASCIIString -> [ASCIIString])

-- >>> correctness1
-- +++ OK, passed 1000 tests.
correctness1 :: IO ()
correctness1 = quickCheck $ \(chunks :: [Chunk]) -> coerce (decode1 . encode1) chunks === chunks

-- >>> correctness2
-- +++ OK, passed 300 tests.
-- +++ OK, passed 1 test.
correctness2 :: IO ()
correctness2 = do
    quickCheck . withMaxSuccess 300 $ \(chunks :: [Chunk]) ->
        coerce (decode2 . encode2) chunks === chunks
    quickCheck . withMaxSuccess 1 $ do
        let chunk = repeat $ repeat 'a'
        take 1000 (head . decode2 $ encode2 chunk) === take 1000 (head chunk)

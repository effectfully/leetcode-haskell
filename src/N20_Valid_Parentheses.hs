{-# LANGUAGE MultiWayIf #-}
module N20_Valid_Parentheses where

{-
Given a string s containing just the characters '(', ')', '{', '}', '[' and ']', determine if the
input string is valid.

An input string is valid if:

    Open brackets must be closed by the same type of brackets.
    Open brackets must be closed in the correct order.
    Every close bracket has a corresponding open bracket of the same type.

Example 1:

Input: s = "()"
Output: true

Example 2:

Input: s = "()[]{}"
Output: true

Example 3:

Input: s = "(]"
Output: false

Constraints:

    1 <= s.length <= 10^4
    s consists of parentheses only '()[]{}'.
-}

parenTable :: [(Char, Char)]
parenTable = [('(', ')'), ('{', '}'), ('[', ']')]

-- >>> direct ""
-- True
-- >>> direct "("
-- False
-- >>> direct "()"
-- True
-- >>> direct "([)"
-- False
-- >>> direct "([)]"
-- False
-- >>> direct "([])"
-- True
-- >>> direct "()[]{}"
-- True
-- >>> direct "({}))"
-- False
-- >>> direct "({([[{()[]}]]{[](){}})((()))}[])"
-- True
direct :: String -> Bool
direct = go [] where
    go ctx ""      = null ctx
    go ctx (c:str) = if
        | Just c' <- lookup c parenTable -> go (c':ctx) str
        | c `elem` map snd parenTable -> case ctx of
              c' : ctx' | c == c' -> go ctx' str
              _                   -> False
        | otherwise -> go ctx str  -- We could just throw here.

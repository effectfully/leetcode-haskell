module N36_Valid_Sudoku where

{-
Determine if a 9 x 9 Sudoku board is valid. Only the filled cells need to be validated according to the following rules:

    Each row must contain the digits 1-9 without repetition.
    Each column must contain the digits 1-9 without repetition.
    Each of the nine 3 x 3 sub-boxes of the grid must contain the digits 1-9 without repetition.

Note:

    A Sudoku board (partially filled) could be valid but is not necessarily solvable.
    Only the filled cells need to be validated according to the mentioned rules.

Example 1:

Input: board =
[["5","3",".",".","7",".",".",".","."]
,["6",".",".","1","9","5",".",".","."]
,[".","9","8",".",".",".",".","6","."]
,["8",".",".",".","6",".",".",".","3"]
,["4",".",".","8",".","3",".",".","1"]
,["7",".",".",".","2",".",".",".","6"]
,[".","6",".",".",".",".","2","8","."]
,[".",".",".","4","1","9",".",".","5"]
,[".",".",".",".","8",".",".","7","9"]]
Output: true

Example 2:

Input: board =
[["8","3",".",".","7",".",".",".","."]
,["6",".",".","1","9","5",".",".","."]
,[".","9","8",".",".",".",".","6","."]
,["8",".",".",".","6",".",".",".","3"]
,["4",".",".","8",".","3",".",".","1"]
,["7",".",".",".","2",".",".",".","6"]
,[".","6",".",".",".",".","2","8","."]
,[".",".",".","4","1","9",".",".","5"]
,[".",".",".",".","8",".",".","7","9"]]
Output: false
Explanation: Same as Example 1, except with the 5 in the top left corner being modified to 8. Since there are two 8's in the top left 3x3 sub-box, it is invalid.

Constraints:

    board.length == 9
    board[i].length == 9
    board[i][j] is a digit 1-9 or '.'.
-}

import Data.Hashable (Hashable)
import Data.HashMap.Strict qualified as HashMap
import Data.List

allUnique :: (Hashable a, Eq a) => [a] -> Bool
allUnique = all (== 1) . HashMap.fromListWith (+) . map (\x -> (x, 1 :: Int))

isValidPart :: [String] -> Bool
isValidPart xs
    | length xs == 9 = allUnique $ filter (/= ".") xs
    | otherwise      = error "wrong solution"

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = go where
    go [] = []
    go xs = case splitAt n xs of
        (prefix, suffix) -> prefix : go suffix

-- >>> let board = [["5","3",".",".","7",".",".",".","."],["6",".",".","1","9","5",".",".","."],[".","9","8",".",".",".",".","6","."],["8",".",".",".","6",".",".",".","3"],["4",".",".","8",".","3",".",".","1"],["7",".",".",".","2",".",".",".","6"],[".","6",".",".",".",".","2","8","."],[".",".",".","4","1","9",".",".","5"],[".",".",".",".","8",".",".","7","9"]]
-- >>> let parts = boardParts board
-- >>> render = mapM_ $ putStrLn . concat
-- >>> render board
-- 53..7....
-- 6..195...
-- .98....6.
-- 8...6...3
-- 4..8.3..1
-- 7...2...6
-- .6....28.
-- ...419..5
-- ....8..79
-- >>> render $ take 9 parts
-- 53..7....
-- 6..195...
-- .98....6.
-- 8...6...3
-- 4..8.3..1
-- 7...2...6
-- .6....28.
-- ...419..5
-- ....8..79
-- >>> render . take 9 $ drop 9 parts
-- 56.847...
-- 3.9...6..
-- ..8......
-- .1..8..4.
-- 79.6.2.18
-- .5..3..9.
-- ......2..
-- ..6...8.7
-- ...316.59
-- >>> render $ drop 18 parts
-- 56.3.9..8
-- .1.79..5.
-- .....6...
-- 847......
-- .8.6.2.3.
-- ......316
-- ...6.....
-- .4..18.9.
-- 2..8.7.59
boardParts :: [[String]] -> [[String]]
boardParts board = concat
    [ board
    , transpose board
    , map concat . concatMap (chunksOf 3) . map transpose $ chunksOf 3 board
    ]

-- >>> isValidBoard [["5","3",".",".","7",".",".",".","."],["6",".",".","1","9","5",".",".","."],[".","9","8",".",".",".",".","6","."],["8",".",".",".","6",".",".",".","3"],["4",".",".","8",".","3",".",".","1"],["7",".",".",".","2",".",".",".","6"],[".","6",".",".",".",".","2","8","."],[".",".",".","4","1","9",".",".","5"],[".",".",".",".","8",".",".","7","9"]]
-- True
-- >>> isValidBoard [["8","3",".",".","7",".",".",".","."],["6",".",".","1","9","5",".",".","."],[".","9","8",".",".",".",".","6","."],["8",".",".",".","6",".",".",".","3"],["4",".",".","8",".","3",".",".","1"],["7",".",".",".","2",".",".",".","6"],[".","6",".",".",".",".","2","8","."],[".",".",".","4","1","9",".",".","5"],[".",".",".",".","8",".",".","7","9"]]
-- False
isValidBoard :: [[String]] -> Bool
isValidBoard = all isValidPart . boardParts

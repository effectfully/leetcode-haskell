module Lib.Utils
    ( module QuickCheck
    , quickCheck
    , allEqual
    ) where

import Test.QuickCheck as QuickCheck hiding (quickCheck)

quickCheck :: Testable prop => prop -> IO ()
quickCheck prop = do
    let args = stdArgs
            { chatty = False
            , maxSuccess = 1000
            , maxSize = 1000
            }
    result <- quickCheckWithResult args prop
    putStrLn $ output result

allEqual :: (Show a, Eq a) => [a] -> Property
allEqual xs = conjoin $ zipWith (===) xs $ tail xs

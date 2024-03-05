module Utils
(
    append,
    toInt,
    toFloat,
    isInt,
    isFloat
) where

import Data.Maybe
import Text.Read

append :: a -> [a] -> [a]
append e [] = [e]
append e (x:xs) = x:(append e xs)

toInt :: String -> Int
toInt str = read str :: Int

toFloat :: String -> Float
toFloat str = read str :: Float

isInt :: String -> Bool
isInt str = isJust (readMaybe str :: Maybe Int)

isFloat :: String -> Bool
isFloat str = isJust (readMaybe str :: Maybe Float) 

module Utils
(
    append,
    split,
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

split :: String -> String -> [String]
split [] str = [str]
split delims [] = []
split delims str = splitRemainder $ breakBy delims $ shiftDelim delims str where
    breakBy :: String -> String -> (String, String)
    breakBy delims = break (`elem` delims)
    shiftDelim :: String -> String -> String
    shiftDelim _ [] = []
    shiftDelim delims s@(c:rem) = if elem c delims then rem else s
    splitRemainder :: (String, String) -> [String]
    splitRemainder (str, rem) = str:(split delims rem)

toInt :: String -> Int
toInt str = read str :: Int

toFloat :: String -> Float
toFloat str = read str :: Float

isInt :: String -> Bool
isInt str = isJust (readMaybe str :: Maybe Int)

isFloat :: String -> Bool
isFloat str = isJust (readMaybe str :: Maybe Float)

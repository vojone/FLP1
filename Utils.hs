module Utils
(
    trim,
    nd,
    append,
    split,
    toFloats,
    toInt,
    toFloat,
    isInt,
    isFloat
) where

import Data.Maybe
import Data.Char
import qualified Data.Set as Set
import Text.Read

trim :: String -> String
trim = trimRev . trimRev where
    trimRev :: String -> String
    trimRev = reverse . snd . span isSpace

nd :: (Ord a) => [a] -> [a] 
nd = Set.toList . Set.fromList

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

toFloats :: (Integral a) => [a] -> [Float]
toFloats = map toFloat

toFloat :: (Integral a) => a -> Float
toFloat str = fromIntegral str :: Float

toInt :: String -> Int
toInt str = read str :: Int

isInt :: String -> Bool
isInt str = isJust (readMaybe str :: Maybe Int)

isFloat :: String -> Bool
isFloat str = isJust (readMaybe str :: Maybe Float)

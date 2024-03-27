{-
CommonParser module
Contains general functions that are used in tree parser as well as in data parser  

Author: Vojtěch Dvořák (xdvora3o)
-}

module CommonParser
(
    floatp,
    doublep,
    uintp,
    classnamep,
    newlinep,
    finalize
) where

import Data.Char
import Parser

-- Check if character is (part of) the newline or not
isNewline :: Char -> Bool
isNewline = flip elem ['\n', '\r'] 


-- Checks whether the character i allowed in classname or not
isClassnameChar :: Char -> Bool
isClassnameChar c = not ((isSpace c) || (',' == c))


-- | Parses float value
floatp :: ParserCtx Float -> ParserCtx Float
floatp = clearb +++ rule +++ convert read where
    rule :: ParserCtx Float -> ParserCtx Float
    rule = (<?.) ("-" |! "-") +++ (isDigit *|! "digit") +++ -- Integer part
        (<?.) (("." |! ".") +++ (isDigit *|! "digit")) -- Decimal (optional part)


-- | Parses double value
doublep :: ParserCtx Double -> ParserCtx Double
doublep = clearb +++ rule +++ convert read where
    rule :: ParserCtx Double -> ParserCtx Double
    rule = (<?.) ("-" |! "-") +++ (isDigit *|! "digit") +++ -- Integer part
        (<?.) (("." |! ".") +++ (isDigit *|! "digit")) +++ -- Decimal (optional part)
        (<?.) (("e" |! "e") +++ (<?.) (("+" |! "+") <|> ("-" |! "-")) +++ (isDigit *|! "digit")) -- Exponent (Optional too)


-- | Parses unsigned int value
uintp :: ParserCtx Int -> ParserCtx Int
uintp = clearb +++ (isDigit *|! "digit") +++ convert read


-- | Parses classnames
classnamep :: ParserCtx String -> ParserCtx String
classnamep = clearb +++ (isClassnameChar *|! "classname") +++ convert id


-- | Parses newline
newlinep :: (Default a) => ParserCtx a -> ParserCtx a
newlinep = ("\n" |! "newline") <|> ("\r\n" |! "")


-- | Provides conversion from ParserCtx (used inside a parser) to ParserResult
finalize :: ParserCtx a -> ParserResult a
finalize ctx = case ctx of
    ParserCtx{pos=p,str=s,res=(Left ("", exps))} -> Left $ ("Syntax error at " ++ show p ++ -- If there is no custom message make one due to expects
        ": Got " ++ takeWhile isNewline s ++ ", expected one of " ++ show exps, exps)
    ParserCtx{pos=p,res=(Left err@(msg, exps))} -> Left ("Error at " ++ show p ++ -- If there is custom error message preserve it
        ": " ++ msg, exps)
    ParserCtx{pos=p,str=s,res=r@(Right _)} -> if null $ dropWhile isSpace s -- Check if there are some unparsed non prinateble whitespaces at the end (for better robustness)
        then r -- Nice, everything OK!
        else Left ("Syntax error at " ++ show p ++
            ": Unable to parse " ++ takeWhile isNewline s, []) -- If there is something else, return an error


-- select1 :: (a, b, c) -> a
-- select1 (x, _, _) = x

-- select2 :: (a, b, c) -> b
-- select2 (_, x, _) = x

-- select3 :: (a, b, c) -> c
-- select3 (_, _, x) = x

-- trim :: String -> String
-- trim = trimRev . trimRev where
--     trimRev :: String -> String
--     trimRev = reverse . snd . span isSpace

-- nd :: (Ord a) => [a] -> [a]
-- nd = Set.toList . Set.fromList

-- append :: a -> [a] -> [a]
-- append e [] = [e]
-- append e (x:xs) = x:(append e xs)

-- split :: String -> String -> [String]
-- split [] str = [str]
-- split delims [] = []
-- split delims str = splitRemainder $ breakBy delims $ shiftDelim delims str where
--     breakBy :: String -> String -> (String, String)
--     breakBy delims = break (`elem` delims)
--     shiftDelim :: String -> String -> String
--     shiftDelim _ [] = []
--     shiftDelim delims s@(c:rem) = if elem c delims then rem else s
--     splitRemainder :: (String, String) -> [String]
--     splitRemainder (str, rem) = str:(split delims rem)

-- toFloats :: (Integral a) => [a] -> [Float]
-- toFloats = map toFloat

-- toFloat :: (Integral a) => a -> Float
-- toFloat str = fromIntegral str :: Float

-- toInt :: String -> Int
-- toInt str = read str :: Int

-- isInt :: String -> Bool
-- isInt str = isJust (readMaybe str :: Maybe Int)

-- isFloat :: String -> Bool
-- isFloat str = isJust (readMaybe str :: Maybe Float)

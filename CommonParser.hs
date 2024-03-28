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
    stringp,
    classnamep,
    newlinep,
    emptyp,
    finalize
) where

import Data.Char
import qualified Data.Set as Set(fromList, toList)
import Parser


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


-- | Parses string value
stringp :: ParserCtx String -> ParserCtx String
stringp = clearb +++ ("\"" |! "\"") +++
    (<*.) (((\c -> not $ elem c ['\\', '\"']) .|! "symbol") <|> 
    ("\\" |! "backslash") +++ ((\_ -> True) .|! "symbol")) +++
    ("\"" |! "\"") +++ convert read


-- | Parses classnames
classnamep :: ParserCtx String -> ParserCtx String
classnamep = clearb +++ (isClassnameChar *|! "classname") +++ convert id


-- | Parses newline
newlinep :: (Default a) => ParserCtx a -> ParserCtx a
newlinep = ("\n" |! "newline") <|> ("\r\n" |! "")


-- | Parses nothing - just stub for empty columns in csv format
emptyp :: (Default a) => ParserCtx a -> ParserCtx a
emptyp = id


-- | Provides conversion from ParserCtx (used inside a parser) to ParserResult
finalize :: ParserCtx a -> ParserResult a
finalize ctx = 
    let showRemStr = takeWhile (not . isSpace)
        mergeErrq = Set.toList . Set.fromList . foldr (\(_, exps) acc -> exps ++ acc) []
    in case ctx of
        ParserCtx{pos=p,res=(Left ("", exps)),errstr=es} -> Left $ ("Syntax error at " ++ show p ++ -- If there is no custom message make one due to expects
            ": Got \"" ++ es ++ "\", expected one of " ++ show exps, exps)
        ParserCtx{pos=p,res=(Left (msg, exps))} -> Left ("Error at " ++ show p ++ -- If there is custom error message preserve it
            ": " ++ msg, exps)
        ParserCtx{pos=p,str=s,res=r@(Right _),errq=q,errstr=es} -> if null $ dropWhile isSpace s -- Check if there are some unparsed non prinateble whitespaces at the end (for better robustness)
            then r -- Nice, everything OK!
            else if null q -- If there is something else, return an error, check the errq to make better suggestion
                then Left ("Syntax error at " ++ show p ++
                    ": Unable to parse \"" ++ showRemStr s ++ "\"", [])
                else Left ("Syntax error at " ++ show p ++
                    ": Got \"" ++ es ++ "\" expected one of " ++ show (mergeErrq q), [])


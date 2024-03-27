{- | 
Parser module
General framework for creating custom parsers

Author: Vojtěch Dvořák (xdvora3o) 
-}

module Parser
(
    ParserErr,
    ParserResult,
    Default(..),
    ParserCtx(..),
    initParserCtx,
    lahead,
    clearb,
    convert,
    convertold,
    raiseParserErr,
    (|>),
    (+++),
    (<|>),
    (<*.),
    (<+.),
    (<?.),
    (<@),
    (|!),
    (*|!),
    (.|!)
) where

import Data.Char
import Data.List
import qualified Data.Set as Set(fromList, toList)

-- | Following functions are framework for creating custom parsers - because there are two
-- formats in flp-fun project (tree, csv), these functions were created to have better reusability
-- of the parsing, better extensibility and to be reusable for other projects
-- NOTE: this approach is probably MASSIVE OVERKILL for flp-fun project, but it was at least fun  

-- EXAMPLE:
-- We want to accept strings /\(\d+\+\d+\)/ (for example: "(1+2)" ), so we can define regular
-- grammar:
-- ```
-- number = ( isDigit *|! "number")
-- 
-- op = tok ( "+" |! "+")
-- 
-- parse = ( "(" |! "(") +++ number +++ op +++ number +++ ( ")" |! ")")
-- ```
--
-- Then we can parse string "(123+245)":
-- ```
-- ctx = initParserCtx "(123+456)" :: ParserCtx (BinaryTree Int) -- BinaryTree is just example
-- parse ctx
-- ```
-- Result:
-- ```
-- ParserCtx {pos = (0,9), str = "", buf = "(123+456)", res = Right EmptyTree}
-- ```
--
-- But if we try "(123+a456)":
-- ```
-- errCtx = initParserCtx "(123+a456)" :: ParserCtx (BinaryTree Int) -- BinaryTree is just example
-- parse errCtx
-- ```
--- Result:
-- ```
-- ParserCtx {pos = (0,5), str = "a456", buf = "(123+", res = Left ("",["number"])}
-- ```
--
-- HOW CAN BE ERRORS DETECTED?
-- ParserCtx structure has Left value in its res member OR there is some leftover in str member


-- | Data type for storing parsing errors, first member of tuple can be used for custom message
-- in the second member can be stored expected tokens
type ParserErr = (String, [String])


-- | Data type of the result of the parsing (error or result type)
type ParserResult a = Either ParserErr a


-- | Members of class default must have defined the default value (e. g. Empty or something)
class Default a where
    defv :: a


-- Some instances of default (mainly for debugging)
instance Default Int where
    defv = 0

instance Default Float where
    defv = 0.0

instance Default Double where
    defv = 0.0

instance Default (Maybe a) where
    defv = Nothing

instance Default [a] where
    defv = []


-- | Structure that represents parser, it stores context during parsing including the result
data ParserCtx a = ParserCtx {
    pos :: (Int, Int), -- Position of the reading head in the input string
    str :: String, -- Unparsed input string
    buf :: String, -- Buffer with read input string (it can be cleared if value is not needed)
    res :: ParserResult a, -- The result of parsing 
    stepcnt :: Int -- Number of succesfully parsed tokens (important for better error msgs)
} deriving (Show)


-- | Type for concrete functions that perform the parsing
type ParseFunc a = ParserCtx a -> ParserCtx a


-- | No duplicate function 
nd :: (Ord a) => [a] -> [a]
nd = Set.toList . Set.fromList -- This is recommended way of implementation in the learnyouahskell book


-- | Initial parserCtx structure
initParserCtx :: (Default a) => String -> ParserCtx a
initParserCtx inputStr = ParserCtx (0,0) inputStr "" (Right $ defv) 0


-- | Updates position of the reading head due to the string that has been read
updatePos :: (Int, Int) -> String -> (Int, Int)
updatePos p readStr = foldr (\x (r, c) -> if x == '\n' then (r + 1, 0) else (r, c + 1)) p readStr


-- | Clears auxiliary buffer in the parsing context
clearb :: ParserCtx a -> ParserCtx a
clearb ctx = ctx {buf=""}


-- | Performs conversion of string in buffer to value in parser result, if there is Left value
-- context is not changed
-- IMPORTANT: Deletes the old result
convert :: (String -> a) -> ParserCtx a -> ParserCtx a
convert convf ctx = case ctx of
    ParserCtx{res=(Left _)} -> ctx
    ParserCtx{res=(Right _), buf=s} -> ctx{res=(Right $ convf s)}


-- | Performs conversion of string in buffer to value in parser result taking into account the 
-- previous value
convertold :: (String -> a -> a) -> ParserCtx a -> ParserCtx a
convertold convf ctx = case ctx of
    ParserCtx{res=(Left _)} -> ctx
    ParserCtx{res=(Right old), buf=s} -> ctx{res=(Right $ convf s old)}


-- | lookahead operator - checks the future characters in the string but does not moves the reading
-- head - can be used for determing the correct alternative from multiple alternatives which have
-- some collisions 
lahead :: ParseFunc a -> ParserCtx a -> ParserCtx a
lahead ftest ctx = case ftest ctx of
    ParserCtx{res=(Right _)} -> ctx
    ParserCtx{res=(Left (msg, exp))} -> ctx{res=(Left (msg, exp))}


infixl 3 |>

-- | Converts the result value of the parser to the value of different type, if there is an error
-- in the context the same context is returned
(|>) :: (Default a, Default b) => ParseFunc a -> (a -> b -> b) -> ParseFunc b
fl |> convf = \ctx -> case ctx of
    lctx@ParserCtx{res=(Left old)} -> case fl $ lctx{res=(Left $ old)} of
        llctx@ParserCtx{res=(Left new)} -> llctx{res=(Left new)}
        rrctx@ParserCtx{res=(Right new)} -> rrctx{res=(Right $ convf new defv)}
    rctx@ParserCtx{res=(Right old)} -> case fl $ rctx{res=(Right $ defv)} of
        llctx@ParserCtx{res=(Left new)} -> llctx{res=(Left new)}
        rrctx@ParserCtx{res=(Right new)} -> rrctx{res=(Right $ convf new old)}



-- | Changes the result of the parsing and adds expected token to the error structure
raiseParserErr :: String -> String -> ParserCtx a -> ParserCtx a
raiseParserErr msg "" ctx@ParserCtx{res=(Right _)} = ctx {res=(Left (msg, []))}
raiseParserErr msg expStr ctx@ParserCtx{res=(Right _)} = ctx {res=(Left (msg, [expStr]))}
raiseParserErr _ _ ctx = ctx


-- | Updates ParserCtx structure when the correct token is found
move :: (Default a) => String -> String -> ParserCtx a -> ParserCtx a
move readStr newStr ctx@ParserCtx{pos=p, buf=b, res=(Left _)} = ctx {
        pos=(updatePos p readStr),
        buf=(b ++ readStr),
        str=newStr,
        res=(Right $ defv)
    }
move readStr newStr ctx@ParserCtx{pos=p, buf=b, stepcnt=s} = ctx {
        pos=(updatePos p readStr),
        buf=(b ++ readStr),
        str=newStr,
        stepcnt=(s + 1)
    }


-- | Shifts prefix that is made by whitespaces
skipSpaces :: String -> String
skipSpaces = snd . span (\c -> isSpace c && (not $ c `elem` "\r\n"))



-- ------ Functions for creating grammar rules --------
infixl 3 +++

-- | Sequence - Example: `parseA +++ parseB` will accept "AB"
(+++) :: ParseFunc a -> ParseFunc a -> ParseFunc a
fl +++ fr = \ctx -> case fl ctx of
    rctx@ParserCtx{res=(Right _)} -> fr rctx
    ParserCtx{res=(Left _)} -> fl ctx -- If there is an error skip the second parsing function


infixl 1 <|>

-- | Alternatives - Example: `parseA <|> parseB` will accept "A" as well as "B"
(<|>) :: ParseFunc a -> ParseFunc a -> ParseFunc a
fl <|> fr = \ctx -> case fl ctx of
    ParserCtx{res=(Right _)} -> fl ctx
    lctx@ParserCtx{res=(Left (lmsg, lexp)),stepcnt=lstep} -> case fr ctx of -- If there is an error try the second alternative
        ParserCtx{res=(Right _)} -> fr ctx
        ParserCtx{res=(Left (rmsg, rexp)),stepcnt=rstep} -> case compare rstep lstep of -- Detemine which alternative came further
            GT -> lctx{res=(Left (rmsg, rexp)),stepcnt=rstep}
            LT -> lctx{res=(Left (lmsg, lexp)),stepcnt=lstep}
            EQ -> lctx{res=(Left (rmsg, nd $ lexp ++ rexp)),stepcnt=rstep} -- If both of them were equally sucessful merge expected tokens


infixr 6 <*.

-- | Iteration -- Example: `<*. parseA` will accept "", "A", "AA"...
(<*.) :: ParseFunc a -> ParseFunc a
(<*.) f = \ctx -> case f ctx of
    rctx@ParserCtx{res=(Right _)} -> ((<*.) f) rctx
    ParserCtx{res=(Left _)} -> ctx -- If there is an error terminate iteration


infixr 6 <+.

-- | Positive iteration -- Example: `<*. parseA` will accept "A", "AA", "AAA"...
(<+.) :: ParseFunc a -> ParseFunc a
(<+.) f = ((<*.) f) . f


infixr 6 <?.

-- | Optional -- Example: `<?. parseA +++ parseB` will accept "B", "AB"
(<?.) :: ParseFunc a -> ParseFunc a
(<?.) f = \ctx -> case f ctx of
    rctx@ParserCtx{res=(Right _)} -> rctx
    ParserCtx{res=(Left _)} -> ctx -- If there is an error return the original ctx


infixr 5 <@

-- | Whitespace skip -- Example: `<@ parseA` will accept "A", " A", "  A", "\tA"...
(<@) :: ParseFunc a -> ParseFunc a
(<@) f = \ctx@ParserCtx {str=s} -> f $ ctx {str=(skipSpaces s)}


-- ---------------------------------------------------

infix 0 >:

-- | Strips given prefix and returns it in tuple with the rest of that string
(>:) :: String -> String -> (String, String)
pref >: haystackStr = case stripPrefix pref haystackStr of
    Just newStr -> (pref, newStr) 
    Nothing -> ("", haystackStr)



infix 0 >?:

-- | Strips prefix defined by function and returns it in tuple with the rest of that string
-- Same as span
(>?:) :: (Char -> Bool) -> String -> (String, String)
(>?:) = span



infix 9 |!

-- | Operator that check if the prefix of the input contains specific token
-- Example: `tok ("  " :! "indent")` accepts "  ", if there is unexpected thing in the prefix
-- "indent" is appended to expected strings
(|!) :: (Default a) => String -> String -> ParserCtx a -> ParserCtx a
(|!) "" expStr ctx@ParserCtx{str=s} = case null s of
    False -> raiseParserErr "" expStr ctx
    True -> ctx
(|!) tokStr expStr ctx@ParserCtx{str=s} = case tokStr >: s of
    ([], _) -> raiseParserErr "" expStr ctx
    (pref, newStr) -> move pref newStr ctx


infix 9 *|!

-- | Operator that check if the prefix of the input contains specific token defined by function
-- Example: `tok ( isDigit :! "number")` accepts "123", if there is unexpected thing in the prefix
-- "number" is appended to expected strings
(*|!) :: (Default a) => (Char -> Bool) -> String -> ParserCtx a -> ParserCtx a
(*|!) strFunc expStr ctx@ParserCtx{str=s} = case strFunc >?: s of
    ([], _) -> raiseParserErr "" expStr ctx
    (pref, newStr) -> move pref newStr ctx


infix 9 .|!

-- | Same as *|!, but only one the first from the prefix is accepted
(.|!) :: (Default a) =>  (Char -> Bool) -> String -> ParserCtx a -> ParserCtx a
(.|!) _ expStr ctx@ParserCtx{str=""} = raiseParserErr "" expStr ctx
(.|!) strFunc expStr ctx@ParserCtx{str=(prefChar:s)} = case strFunc >?: [prefChar] of
    ([], _) -> raiseParserErr "" expStr ctx
    (pref, _) -> move pref s ctx


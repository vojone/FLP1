{- | 
Parser module
The framework for creating custom parsers

Author: Vojtěch Dvořák (xdvora3o) 
-}

module Parser
(
    ParserErr,
    ParserResult,
    Default(..),
    ParserCtx(..),
    initParserCtx,
    (<->>),
    (<|>),
    (<*.>),
    (<+.>),
    (<?.>),
    (<-*>),
    Token(..),
    TokenF(..),
    tok,
    tokf,
    tokf1
) where

import Data.Char
import Data.List

-- | Following functions are framework for creating custom parsers - because there are two
-- formats in flp-fun project (tree, csv), these functions were created to have better reusability
-- of the parsing, better exensibility and to be reusable for other projects
-- NOTE: this approach is probably overkill for flp-fun project, but it was fun  

-- Example of usage:
-- We want to accept strings /\(\d+\+\d+\)/ (for example: "(1+2)" ), so we can define regular
-- grammar:
-- ```
-- number = tokf ( isDigit :?! "number")
-- 
-- op = tok ( "+" :! "+")
-- 
-- parse = tok ( "(" :! "(") <->> number <->> op <->> number <->> tok ( ")" :! ")")
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


-- | Data type for storing parsing errors, first member of tuple can be used for custom message
-- in the second member can be stored expected tokens
type ParserErr = (String, [String])


-- | Data type of the result of the parsing (error or result type)
type ParserResult a = Either ParserErr a


-- | Members of class default must have defined the default value (e. g. Empty or something)
class Default a where
    defv :: a

-- | Structure that represents parser, it stores context during parsing including the result
data ParserCtx a = ParserCtx {
    pos :: (Int, Int), -- Position of the reading head in the input string
    str :: String, -- Unparsed input string
    buf :: String, -- Buffer with read input string (it can be cleared if value is not needed)
    res :: ParserResult a -- The result of parsing 
} deriving (Show)


-- | Type for concrete functions that perform the parsing
type ParseFunc a = ParserCtx a -> ParserCtx a


-- | Initial parserCtx structure
initParserCtx :: (Default a) => String -> ParserCtx a
initParserCtx inputStr = ParserCtx (0,0) inputStr "" (Right $ defv)


-- | Updates position of the reading head due to the string that has been read
updatePos :: (Int, Int) -> String -> (Int, Int)
updatePos p readStr = foldr (\x (r, c) -> if x == '\n' then (r + 1, 0) else (r, c + 1)) p readStr


-- | Clears auxiliary buffer in the parsing context
clearBuf :: ParserCtx a -> ParserCtx a
clearBuf ctx = ctx {buf=""}


-- | Adds expected token to the error structure
addExp :: String -> ParserCtx a -> ParserCtx a
addExp expStr ctx@ParserCtx{res=(Right _)} = ctx {res=(Left ("", [expStr]))}
addExp expStr ctx@ParserCtx{res=(Left (msg, exps))} = ctx {res=(Left (msg, expStr:exps))}


-- | Updates ParserCtx structure when the correct token is found
move :: (Default a) => String -> String -> ParserCtx a -> ParserCtx a
move readStr newStr ctx@ParserCtx{pos=p, buf=b, res=(Left _)} = ctx {
        pos=(updatePos p readStr),
        buf=(b ++ readStr),
        str=newStr,
        res=(Right $ defv)
    }
move readStr newStr ctx@ParserCtx{pos=p, buf=b} = ctx {
        pos=(updatePos p readStr),
        buf=(b ++ readStr),
        str=newStr
    }


-- | Shifts prefix that is made by whitespaces
skipSpaces :: String -> String
skipSpaces = snd . span (\c -> isSpace c && (not $ c `elem` "\r\n"))



-- ------ Functions for creating grammar rules --------
infixl 3 <->>

-- | Sequence - Example: `parseA <->> parseB` will accept "AB"
(<->>) :: ParseFunc a -> ParseFunc a -> ParseFunc a
fl <->> fr = \ctx -> case fl ctx of
    rctx@ParserCtx{res=(Right _)} -> fr rctx
    ParserCtx{res=(Left _)} -> fl ctx -- If there is an error skip the second parsing function


infixl 1 <|>

-- | Alternatives - Example: `parseA <|> parseB` will accept "A" as well as "B"
(<|>) :: ParseFunc a -> ParseFunc a -> ParseFunc a
fl <|> fr = \ctx -> case fl ctx of
    ParserCtx{res=(Right _)} -> fl ctx
    lctx@ParserCtx{res=(Left _)} -> fr lctx -- If there is and error try the second alternative


infixr 6 <*.>

-- | Iteration -- Example: `<*.> parseA` will accept "", "A", "AA"...
(<*.>) :: ParseFunc a -> ParseFunc a
(<*.>) f = \ctx -> case f ctx of
    rctx@ParserCtx{res=(Right _)} -> ((<*.>) f) rctx
    ParserCtx{res=(Left _)} -> ctx -- If there is an error terminate iteration


infixr 6 <+.>

-- | Positive iteration -- Example: `<*.> parseA` will accept "A", "AA", "AAA"...
(<+.>) :: ParseFunc a -> ParseFunc a
(<+.>) f = ((<*.>) f) . f


infixr 6 <?.>

-- | Optional -- Example: `<?.> parseA <->> parseB` will accept "B", "AB"
(<?.>) :: ParseFunc a -> ParseFunc a
(<?.>) f = \ctx -> case f ctx of
    rctx@ParserCtx{res=(Right _)} -> rctx
    ParserCtx{res=(Left _)} -> ctx -- If there is an error return the original ctx


infixr 5 <-*>

-- | Whitespace skip -- Example: `<-*> parseA` will accept "A", " A", "  A", "\tA"...
(<-*>) :: ParseFunc a -> ParseFunc a
(<-*>) f = \ctx@ParserCtx {str=s} -> f $ ctx {str=(skipSpaces s)}


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


-- | Data type for token (for more readable definition of grammar rules), the first string
-- is string than is expected in input, the second string is short textual description of expected
-- string for more readable error messages
infixr 9 :!
data Token = String :! String


infixr 9 :?!
data TokenF = (Char -> Bool) :?! String

-- | Function that check if the prefix of the input contains specific token
-- Example: `tok ("  " :! "indent")` accepts "  ", if there is unexpected thing in the prefix
-- "indent" is appended to expected strings
tok :: (Default a) => Token -> ParserCtx a -> ParserCtx a
tok (tokStr :! expStr) ctx@ParserCtx{str=s} = case tokStr >: s of
    ([], _) -> addExp expStr ctx
    (pref, newStr) -> move pref newStr ctx


-- | Function that check if the prefix of the input contains specific token defined by function
-- Example: `tok ( isDigit :! "number")` accepts "123", if there is unexpected thing in the prefix
-- "number" is appended to expected strings
tokf :: (Default a) => TokenF -> ParserCtx a -> ParserCtx a
tokf (strFunc :?! expStr) ctx@ParserCtx{str=s} = case strFunc >?: s of
    ([], _) -> addExp expStr ctx
    (pref, newStr) -> move pref newStr ctx


-- | Same as tokf, but only one the first from the prefix is accepted
tokf1 :: (Default a) => TokenF -> ParserCtx a -> ParserCtx a
tokf1 (_ :?! expStr) ctx@ParserCtx{str=""} = addExp expStr ctx
tokf1 (strFunc :?! expStr) ctx@ParserCtx{str=(prefChar:s)} = case strFunc >?: [prefChar] of
    ([], _) -> addExp expStr ctx
    (pref, _) -> move pref s ctx

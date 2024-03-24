module Parser
(
    -- parse,
    BinaryDecisionTree,
    BinaryTree(..),
    DecisionData(..)
) where

import Data.Char
import Data.List

import Utils

data BinaryTree a =
    Empty |
    Node a (BinaryTree a) (BinaryTree a) |
    Leaf a

instance Default (BinaryTree a) where
    defv = Empty

instance (Show a) => Show (BinaryTree a) where
    show tree = showRec 0 tree where
        showRec nestLvl n = case n of
            (Node d l r) -> nest nestLvl ++ "Node: " ++ show d ++ "\n" ++ showRec (nestLvl + 1) l ++ showRec (nestLvl + 1) r
            (Leaf d) -> nest nestLvl ++ "Leaf: " ++ show d ++ "\n"
            (Empty) -> nest nestLvl ++ "Empty" ++ "\n"
        nest i = concat $ replicate i "  "

data DecisionData = 
    Decision { index :: Int, threshold :: Float } |
    Class { name :: String }

instance Show DecisionData where
    show decData = case decData of
        (Decision i t) -> show i ++ ", " ++ show t
        (Class c) -> id c

type BinaryDecisionTree = BinaryTree DecisionData

-- data DecisionTreeLine =
--     None |
--     NodeLine { indent :: Int, dataIndex :: Int, valThreshold :: Float } |
--     LeafLine { indent :: Int, className :: String }
--     deriving (Eq, Show)



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
(<->>) :: (ParserCtx a -> ParserCtx a) -> (ParserCtx a -> ParserCtx a) -> (ParserCtx a -> ParserCtx a)
fl <->> fr = \ctx -> case fl ctx of
    rctx@ParserCtx{res=(Right _)} -> fr rctx
    ParserCtx{res=(Left _)} -> fl ctx -- If there is an error skip the second parsing function


infixl 1 <|>

-- | Alternatives - Example: `parseA <|> parseB` will accept "A" as well as "B"
(<|>) :: (ParserCtx a -> ParserCtx a) -> (ParserCtx a -> ParserCtx a) -> (ParserCtx a -> ParserCtx a)
fl <|> fr = \ctx -> case fl ctx of
    ParserCtx{res=(Right _)} -> fl ctx
    lctx@ParserCtx{res=(Left _)} -> fr lctx -- If there is and error try the second alternative


infixr 5 <*.>

-- | Iteration -- Example: `<*.> parseA` will accept "", "A", "AA"...
(<*.>) :: (ParserCtx a -> ParserCtx a) -> (ParserCtx a -> ParserCtx a)
(<*.>) f = \ctx -> case f ctx of
    rctx@ParserCtx{res=(Right _)} -> ((<*.>) f) rctx
    ParserCtx{res=(Left _)} -> ctx -- If there is an error terminate iteration


infixr 6 <+.>

-- | Positive iteration -- Example: `<*.> parseA` will accept "A", "AA", "AAA"...
(<+.>) :: (ParserCtx a -> ParserCtx a) -> (ParserCtx a -> ParserCtx a)
(<+.>) f = ((<*.>) f) . f


infixr 6 <-*>

-- | Whitespace skip -- Example: `<-*> parseA` will accept "A", " A", "  A", "\tA"...
(<-*>) :: (ParserCtx a -> ParserCtx a) -> (ParserCtx a -> ParserCtx a)
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
infixl 9 :!
data Token = String :! String


infixl 9 :?!
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


indent :: ParserCtx (BinaryTree a) -> ParserCtx (BinaryTree a)
indent = tok ("  " :! "indent")

-- nodeKw :: ParserCtx (BinaryTree a) -> ParserCtx (BinaryTree a)
-- nodeKw ctx@ParserCtx{pos=p, str=s} = <!> "Node" "Node"

-- leafKw :: ParserCtx (BinaryTree a) -> ParserCtx (BinaryTree a)
-- leafKw ctx@ParserCtx{pos=p, str=s} = <!> "Leaf" "Leaf"

-- parse :: [String] -> TreeParserResult
-- parse ::

-- indent :: (ParserCtx -> ParserCtx)
-- indent p@ParserCtx{pos=(r,c), str=s} = if isPrefixOf "  " s
--     then 

-- isNewline :: Char -> Bool
-- isNewline = flip elem ['\n', '\r'] 

-- skipNewline :: String -> String
-- skipNewline [] = []
-- skipNewline (c1:[])
--     | c1 == '\n' = []
--     | otherwise = c1:[]
-- skipNewline (c1:c2:str)
--     | c1 == '\n' = c2:str
--     | c1 == '\r' && c2 == '\n' = str
--     | otherwise = c1:c2:str


-- data ParserState =
--     Init |
--     Indentation |
--     NodeKeyword |
--     LeafKeyword |
--     LeafColon |
--     NodeColon |
--     NodeIndex |
--     NodeIndexDigit |
--     NodeThreshold |
--     NodeThresholdIntPart |
--     NodeThresholdDecPart |
--     LeafClassChar |
--     LeafClass |
--     NodeComma |
--     LineEnd |
--     Final
--     deriving (Eq, Show)

-- onStackIs :: (Eq a) => [a] -> [a] -> Bool
-- onStackIs stack = (==) $ reverse stack

-- onStackIsInt :: String -> Bool
-- onStackIsInt stack = isInt $ reverse stack

-- onStackIsFloat :: String -> Bool
-- onStackIsFloat stack = isFloat $ reverse stack

-- onStackIsWhitespace :: String -> Bool
-- onStackIsWhitespace (c:[]) = isSpace c && (not $ elem c ['\n', '\r'])
-- onStackIsWhitespace _ = False

-- onStackIsNewline :: String -> Bool
-- onStackIsNewline stack = onStackIs stack "\n" || onStackIs stack "\r\n"

-- hasState :: ParserState -> [ParserState] -> Bool
-- hasState state = elem state


-- data TokenType =
--     Indent |
--     NodeToken |
--     LeafToken |
--     ClassName |
--     Index |
--     Threshold
--     deriving (Eq, Show)

-- data Token = Token TokenType String deriving (Show)

-- parseLineRec :: ([Token], (String, String, ParserState)) -> ([Token], (String, String, ParserState))
-- parseLineRec (tokens, (stack, [], state))
--     | state == Final = (tokens, (stack, [], state))
--     | state /= Final = error $ "Unexpected token! Unable to parse: '" ++ reverse stack ++ "' (" ++ show state ++ ")"
-- parseLineRec (tokens, (stack, c:str, state))
--     | onStackIs "  " stack && state == Init = parseLineRec (append (Token Indent (reverse stack)) tokens, (c:[], str, Indentation))
--     | onStackIs "  " stack && state == Indentation = parseLineRec (append (Token Indent (reverse stack)) tokens, (c:[], str, Indentation))
--     | onStackIs "Node" stack && state == Init = parseLineRec (append (Token NodeToken (reverse stack)) tokens, (c:[], str, NodeKeyword))
--     | onStackIs "Node" stack && state == Indentation = parseLineRec (append (Token NodeToken (reverse stack)) tokens, (c:[], str, NodeKeyword))
--     | onStackIs "Leaf" stack && state == Init = parseLineRec (append (Token LeafToken (reverse stack)) tokens, (c:[], str, LeafKeyword))
--     | onStackIs "Leaf" stack && state == Indentation = parseLineRec (append (Token LeafToken (reverse stack)) tokens, (c:[], str, LeafKeyword))
--     | onStackIs stack ":" && state == NodeKeyword = parseLineRec (tokens, (c:[], str, NodeColon))
--     | state == NodeColon && (isDigit c || c == '-') = parseLineRec (tokens, (c:[], str, NodeIndexDigit))
--     | state == NodeIndexDigit && isDigit c = parseLineRec (tokens, (c:stack, str, NodeIndexDigit))
--     | state == NodeIndexDigit && (not $ isDigit c) = parseLineRec (append (Token Index (reverse stack)) tokens, (c:[], str, NodeIndex))
--     | onStackIs "," stack && state == NodeIndex = parseLineRec (tokens, (c:[], str, NodeComma))
--     | state == NodeComma && (isDigit c || c == '-') = parseLineRec (tokens, (c:[], str, NodeThresholdIntPart))
--     | state == NodeThresholdIntPart && (isDigit c) = parseLineRec (tokens, (c:stack, str, NodeThresholdIntPart))
--     | state == NodeThresholdIntPart && (c == '.') = parseLineRec (tokens, (c:stack, str, NodeThresholdDecPart))
--     | state == NodeThresholdDecPart && (isDigit c) = parseLineRec (tokens, (c:stack, str, NodeThresholdDecPart))
--     | hasState state [NodeThresholdDecPart, NodeThresholdIntPart] && (not $ isDigit c) = parseLineRec (append (Token Threshold (reverse stack)) tokens, (c:[], str, NodeThreshold))
--     | onStackIs ":" stack && state == LeafKeyword = parseLineRec (tokens, (c:[], str, LeafColon))
--     | state == LeafColon && (isLetter c) = parseLineRec (tokens, (c:[], str, LeafClassChar))
--     | state == LeafClassChar && (isLetter c) = parseLineRec (tokens, (c:stack, str, LeafClassChar))
--     | state == LeafClassChar && (not $ isLetter c) = parseLineRec (append (Token ClassName (reverse stack)) tokens, (c:[], str, LeafClass))
--     | onStackIsNewline stack && hasState state [NodeThreshold, LeafClass] && c == '$' = parseLineRec (tokens, ([], str, Final))
--     | onStackIsWhitespace stack && hasState state [NodeKeyword, NodeColon, NodeIndex, NodeComma, NodeThreshold, LeafKeyword, LeafColon, LeafClass] = parseLineRec (tokens, (c:[], str, state))
--     | otherwise = parseLineRec (tokens, (c:stack, str, state))

-- parseLine :: String -> DecisionTreeLine
-- parseLine str' = 
--     let parseLine' str = parseLineRec ([], ("", (append '$' . append '\n') str, Init))
--     in toTreeLine $ fst $ parseLine' str'


-- toTreeLine :: [Token] -> DecisionTreeLine
-- toTreeLine tokens = initializeLine (getLineInit tokens) tokens where
--     getLineInit :: [Token] -> DecisionTreeLine
--     getLineInit = foldl (\a t -> if a == None then getLineInit' t else a) None
--     getLineInit' :: Token -> DecisionTreeLine
--     getLineInit' (Token ttype _) = case ttype of
--         NodeToken -> NodeLine 0 0 0.0
--         LeafToken -> LeafLine 0 ""
--         _ -> None
--     initializeLine :: DecisionTreeLine -> [Token] -> DecisionTreeLine
--     initializeLine None [] = None
--     initializeLine tline [] = tline
--     initializeLine l@(LeafLine i cls) ((Token ttype v):tokens) = case ttype of
--         Indent -> initializeLine (LeafLine (i + 1) cls) tokens
--         ClassName -> initializeLine (LeafLine i v) tokens
--         _ -> initializeLine l tokens
--     initializeLine l@(NodeLine i idx t) ((Token ttype v):tokens) = case ttype of
--         Indent -> initializeLine (NodeLine (i + 1) idx t) tokens
--         Index -> initializeLine (NodeLine i (toInt v) t) tokens
--         Threshold -> initializeLine (NodeLine i idx (read v :: Float)) tokens
--         _ -> initializeLine l tokens


-- validateLineRec :: Int -> Int -> [DecisionTreeLine] -> [DecisionTreeLine]
-- validateLineRec _ 0 [] = []
-- validateLineRec _ _ [] = error "Missing child!"
-- validateLineRec lvl childn ((LeafLine i _):tlines)
--     | lvl /= i = error $ "Indentation! Exp.: " ++ show lvl ++ " Got: " ++ show i
--     | otherwise = tlines
-- validateLineRec lvl childn ((NodeLine i idx threshold):tlines)
--     | lvl /= i = error $ "Indentation! Exp.: " ++ show lvl ++ " Got:" ++ show i
--     | idx < 0 = error "Negative index!"
--     | otherwise = snd $ until (\(x, y) -> x == 0) validateChild (childn, tlines) where
--         validateChild (remChildn, tlines) = (remChildn - 1, validateLineRec (lvl + 1) childn tlines)


-- validateTree :: [DecisionTreeLine] -> [DecisionTreeLine]
-- validateTree tlines = case validateLineRec 0 2 tlines of
--     [] -> tlines
--     _ -> error "There are more standalone trees in the input!"


-- buildTreeRec :: Int -> [DecisionTreeLine] -> ([DecisionTreeLine], BinaryDecisionTree)
-- buildTreeRec _ [] = ([], Empty)
-- buildTreeRec nchild (tline:tlines) = case tline of
--     (LeafLine _ className) -> (tlines, Leaf (Class className))
--     (NodeLine _ idx threshold) -> ((fst $ buildRight nchild tlines), buildNode idx threshold) where
--         buildNode idx threshold = Node (Decision idx threshold) (snd $ buildLeft nchild tlines) (snd $ buildRight nchild tlines)
--         buildLeft nchild tlines = buildTreeRec nchild tlines 
--         buildRight nchild tlines = buildLeft nchild $ fst $ buildLeft nchild tlines


-- buildTree :: [DecisionTreeLine] -> BinaryDecisionTree
-- buildTree tlines = snd $ buildTreeRec 2 tlines


-- parse :: String -> BinaryDecisionTree
-- parse str = buildTree . validateTree . map parseLine $ lines str



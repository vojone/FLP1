module TreeParser
(
    parse,
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

instance (Show a) => Show (BinaryTree a) where
    show tree = showRec 0 tree where
        showRec nestLvl n = case n of
            (Node d l r) -> nest nestLvl ++ "Node: " ++ show d ++ "\n" ++ showRec (nestLvl + 1) l ++ showRec (nestLvl + 1) r
            (Leaf d) -> nest nestLvl ++ "Leaf: " ++ show d ++ "\n"
        nest i = unwords $ replicate i "  "

data DecisionData = 
    Decision { index :: Int, threshold :: Float } |
    Class { name :: String }

instance Show DecisionData where
    show decData = case decData of
        (Decision i t) -> show i ++ ", " ++ show t
        (Class c) -> id c

type BinaryDecisionTree = BinaryTree DecisionData

data DecisionTreeLine =
    None |
    NodeLine { indent :: Int, dataIndex :: Int, valThreshold :: Float } |
    LeafLine { indent :: Int, className :: String }
    deriving (Eq, Show)

isNewline :: Char -> Bool
isNewline = flip elem ['\n', '\r'] 

skipNewline :: String -> String
skipNewline [] = []
skipNewline (c1:[])
    | c1 == '\n' = []
    | otherwise = c1:[]
skipNewline (c1:c2:str)
    | c1 == '\n' = c2:str
    | c1 == '\r' && c2 == '\n' = str
    | otherwise = c1:c2:str


data ParserState =
    Init |
    Indentation |
    NodeKeyword |
    LeafKeyword |
    LeafColon |
    NodeColon |
    NodeIndex |
    NodeIndexDigit |
    NodeThreshold |
    NodeThresholdIntPart |
    NodeThresholdDecPart |
    LeafClassChar |
    LeafClass |
    NodeComma |
    LineEnd |
    Final
    deriving (Eq, Show)

onStackIs :: (Eq a) => [a] -> [a] -> Bool
onStackIs stack = (==) $ reverse stack

onStackIsInt :: String -> Bool
onStackIsInt stack = isInt $ reverse stack

onStackIsFloat :: String -> Bool
onStackIsFloat stack = isFloat $ reverse stack

onStackIsWhitespace :: String -> Bool
onStackIsWhitespace (c:[]) = isSpace c && (not $ elem c ['\n', '\r'])
onStackIsWhitespace _ = False

onStackIsNewline :: String -> Bool
onStackIsNewline stack = onStackIs stack "\n" || onStackIs stack "\r\n"

hasState :: ParserState -> [ParserState] -> Bool
hasState state = elem state


data TokenType =
    Indent |
    NodeToken |
    LeafToken |
    ClassName |
    Index |
    Threshold
    deriving (Eq, Show)

data Token = Token TokenType String deriving (Show)

parseLineRec :: ([Token], (String, String, ParserState)) -> ([Token], (String, String, ParserState))
parseLineRec (tokens, (stack, [], state))
    | state == Final = (tokens, (stack, [], state))
    | state /= Final = error $ "Unexpected token! Unable to parse: '" ++ reverse stack ++ "' (" ++ show state ++ ")"
parseLineRec (tokens, (stack, c:str, state))
    | onStackIs "  " stack && state == Init = parseLineRec (append (Token Indent (reverse stack)) tokens, (c:[], str, Indentation))
    | onStackIs "  " stack && state == Indentation = parseLineRec (append (Token Indent (reverse stack)) tokens, (c:[], str, Indentation))
    | onStackIs "Node" stack && state == Init = parseLineRec (append (Token NodeToken (reverse stack)) tokens, (c:[], str, NodeKeyword))
    | onStackIs "Node" stack && state == Indentation = parseLineRec (append (Token NodeToken (reverse stack)) tokens, (c:[], str, NodeKeyword))
    | onStackIs "Leaf" stack && state == Init = parseLineRec (append (Token LeafToken (reverse stack)) tokens, (c:[], str, LeafKeyword))
    | onStackIs "Leaf" stack && state == Indentation = parseLineRec (append (Token LeafToken (reverse stack)) tokens, (c:[], str, LeafKeyword))
    | onStackIs stack ":" && state == NodeKeyword = parseLineRec (tokens, (c:[], str, NodeColon))
    | state == NodeColon && (isDigit c || c == '-') = parseLineRec (tokens, (c:[], str, NodeIndexDigit))
    | state == NodeIndexDigit && isDigit c = parseLineRec (tokens, (c:stack, str, NodeIndexDigit))
    | state == NodeIndexDigit && (not $ isDigit c) = parseLineRec (append (Token Index (reverse stack)) tokens, (c:[], str, NodeIndex))
    | onStackIs "," stack && state == NodeIndex = parseLineRec (tokens, (c:[], str, NodeComma))
    | state == NodeComma && (isDigit c || c == '-') = parseLineRec (tokens, (c:[], str, NodeThresholdIntPart))
    | state == NodeThresholdIntPart && (isDigit c) = parseLineRec (tokens, (c:stack, str, NodeThresholdIntPart))
    | state == NodeThresholdIntPart && (c == '.') = parseLineRec (tokens, (c:stack, str, NodeThresholdDecPart))
    | state == NodeThresholdDecPart && (isDigit c) = parseLineRec (tokens, (c:stack, str, NodeThresholdDecPart))
    | hasState state [NodeThresholdDecPart, NodeThresholdIntPart] && (not $ isDigit c) = parseLineRec (append (Token Threshold (reverse stack)) tokens, (c:[], str, NodeThreshold))
    | onStackIs ":" stack && state == LeafKeyword = parseLineRec (tokens, (c:[], str, LeafColon))
    | state == LeafColon && (isLetter c) = parseLineRec (tokens, (c:[], str, LeafClassChar))
    | state == LeafClassChar && (isLetter c) = parseLineRec (tokens, (c:stack, str, LeafClassChar))
    | state == LeafClassChar && (not $ isLetter c) = parseLineRec (append (Token ClassName (reverse stack)) tokens, (c:[], str, LeafClass))
    | onStackIsNewline stack && hasState state [NodeThreshold, LeafClass] && c == '$' = parseLineRec (tokens, ([], str, Final))
    | onStackIsWhitespace stack && hasState state [NodeKeyword, NodeColon, NodeIndex, NodeComma, NodeThreshold, LeafKeyword, LeafColon, LeafClass] = parseLineRec (tokens, (c:[], str, state))
    | otherwise = parseLineRec (tokens, (c:stack, str, state))

parseLine :: String -> DecisionTreeLine
parseLine str' = 
    let parseLine' str = parseLineRec ([], ("", (append '$' . append '\n') str, Init))
    in toTreeLine $ fst $ parseLine' str'


toTreeLine :: [Token] -> DecisionTreeLine
toTreeLine tokens = initializeLine (getLineInit tokens) tokens where
    getLineInit :: [Token] -> DecisionTreeLine
    getLineInit = foldl (\a t -> if a == None then getLineInit' t else a) None
    getLineInit' :: Token -> DecisionTreeLine
    getLineInit' (Token ttype _) = case ttype of
        NodeToken -> NodeLine 0 0 0.0
        LeafToken -> LeafLine 0 ""
        _ -> None
    initializeLine :: DecisionTreeLine -> [Token] -> DecisionTreeLine
    initializeLine None [] = None
    initializeLine tline [] = tline
    initializeLine l@(LeafLine i cls) ((Token ttype v):tokens) = case ttype of
        Indent -> initializeLine (LeafLine (i + 1) cls) tokens
        ClassName -> initializeLine (LeafLine i v) tokens
        _ -> initializeLine l tokens
    initializeLine l@(NodeLine i idx t) ((Token ttype v):tokens) = case ttype of
        Indent -> initializeLine (NodeLine (i + 1) idx t) tokens
        Index -> initializeLine (NodeLine i (toInt v) t) tokens
        Threshold -> initializeLine (NodeLine i idx (toFloat v)) tokens
        _ -> initializeLine l tokens


validateLineRec :: Int -> Int -> [DecisionTreeLine] -> [DecisionTreeLine]
validateLineRec _ 0 [] = []
validateLineRec _ _ [] = error "Missing child!"
validateLineRec lvl childn ((LeafLine i _):tlines)
    | lvl /= i = error $ "Indentation! Exp.: " ++ show lvl ++ " Got: " ++ show i
    | otherwise = tlines
validateLineRec lvl childn ((NodeLine i idx threshold):tlines)
    | lvl /= i = error $ "Indentation! Exp.: " ++ show lvl ++ " Got:" ++ show i
    | idx < 0 = error "Negative index!"
    | otherwise = snd $ until (\(x, y) -> x == 0) validateChild (childn, tlines) where
        validateChild (remChildn, tlines) = (remChildn - 1, validateLineRec (lvl + 1) childn tlines)


validateTree :: [DecisionTreeLine] -> [DecisionTreeLine]
validateTree tlines = case validateLineRec 0 2 tlines of
    [] -> tlines
    _ -> error "There are more standalone trees in the input!"


buildTreeRec :: Int -> [DecisionTreeLine] -> ([DecisionTreeLine], BinaryDecisionTree)
buildTreeRec _ [] = ([], Empty)
buildTreeRec nchild (tline:tlines) = case tline of
    (LeafLine _ className) -> (tlines, Leaf (Class className))
    (NodeLine _ idx threshold) -> ((fst $ buildRight nchild tlines), buildNode idx threshold) where
        buildNode idx threshold = Node (Decision idx threshold) (snd $ buildLeft nchild tlines) (snd $ buildRight nchild tlines)
        buildLeft nchild tlines = buildTreeRec nchild tlines 
        buildRight nchild tlines = buildLeft nchild $ fst $ buildLeft nchild tlines


buildTree :: [DecisionTreeLine] -> BinaryDecisionTree
buildTree tlines = snd $ buildTreeRec 2 tlines


parse :: String -> BinaryDecisionTree
parse str = buildTree . validateTree . map parseLine $ lines str



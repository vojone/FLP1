{- | 
BinaryDecisionTreeParser module
Module containing implemantion of parser for binary decision trees, parsing is implemented via
Parser module

Author: Vojtěch Dvořák (xdvora3o) 
-}

module BinaryDecisionTreeParser
(
    parse,
    BinaryDecisionTree,
    DecisionData(..)
) where

import Parser
import DecisionTree
import Data.Char


-- | Structure for storing decision data during parsing, these data are used for building nodes
-- in a decision tree
data TempTree = 
    TNode {
        tindex :: Int,
        tthreshold :: Float,
        tleft :: BinaryDecisionTree,
        tright :: BinaryDecisionTree
    } |
    TLeaf {
        tname :: String
    }

instance Default TempTree where
    defv = TNode{tindex=defv, tthreshold=defv, tleft=defv, tright=defv}


-- | Converts TempTree structure (used during parsing) to BinaryDecisionTree
buildtree :: TempTree -> BinaryDecisionTree -> BinaryDecisionTree
buildtree TNode{tindex=ti, tthreshold=tt, tleft=tl, tright=tr} _ = Node (Decision ti tt) tl tr
buildtree TLeaf{tname=tn} _ = Leaf (Class tn)


-- | Parses float value
floatp :: ParserCtx Float -> ParserCtx Float
floatp = clearb +++ rule +++ convert read where
    rule :: ParserCtx Float -> ParserCtx Float
    rule = (<?.) ("-" |! "-") +++ (isDigit *|! "digit") +++ -- Integer part
        (<?.) (("." |! ".") +++ (isDigit *|! "digit")) -- Decimal (optional part)


-- | Parses int value
intp :: ParserCtx Int -> ParserCtx Int
intp = clearb +++ (<?.) ("-" |! "-") +++ (isDigit *|! "digit") +++ convert read


-- | Parses newline
newlinep :: (Default a) => ParserCtx a -> ParserCtx a
newlinep = ("\n" |! "newline") <|> ("\r\n" |! "")


-- | Parses classnames
classnamep :: ParserCtx String -> ParserCtx String
classnamep = clearb +++ (<?.) (isAlphaNum *|! "classname") +++ convert id


-- | Parses indentation
indentp :: (Default a) => Int -> ParserCtx a -> ParserCtx a
indentp 0 ctx = ctx
indentp n ctx = (("  " |! "indent") +++ indentp (n - 1)) ctx


-- | Parses leaf
leafp :: Int -> ParserCtx BinaryDecisionTree -> ParserCtx BinaryDecisionTree
leafp lvl = (indentp lvl) +++ ("Leaf" |! "Leaf") +++ -- Parses indentation and Leaf keyword
    (<@) (":" |! ":") +++ (<@) (classnamep |> (\c _ -> TLeaf{tname=c})) -- ":" `classname`
    |> buildtree


-- | Parses node and its child
nodep :: Int -> ParserCtx BinaryDecisionTree -> ParserCtx BinaryDecisionTree
nodep lvl = (indentp lvl) +++ ("Node" |! "Node") +++ -- Parses indentation and Node keyword
    (<@) (":" |! ":") +++ (<@) (intp |> (\i d -> d{tindex=i})) +++ -- ":" `feature_index`
    (<@) ("," |! ",") +++ (<@) (floatp |> (\t d -> d{tthreshold=t})) +++ -- "," `float_index`
    (<+.) newlinep +++
    (((nodep $ lvl + 1) <|> (leafp $ lvl + 1)) |> (\t d -> d{tleft=t})) +++ -- Left child
    (<+.) newlinep +++ 
    (((nodep $ lvl + 1) <|> (leafp $ lvl + 1)) |> (\t d -> d{tright=t})) -- Right child
    |> buildtree


-- | Parses a decision tree
treep :: ParserCtx BinaryDecisionTree -> ParserCtx BinaryDecisionTree
treep = ((nodep 0) <|> (leafp 0)) +++ (<*.) newlinep


-- | Wrapper above treep
parse :: String -> ParserResult BinaryDecisionTree
parse str = res $ treep $ initParserCtx str


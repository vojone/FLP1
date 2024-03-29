{- | 
DecisionTreeParser module
Module containing implemantion of parser for binary decision trees, parsing is implemented via
Parser module

Author: Vojtěch Dvořák (xdvora3o) 
-}

module DecisionTreeParser
(
    parseTree,
    BinaryDecisionTree,
    DecisionData(..)
) where

import Parser
import CommonParser
import DecisionTree


-- | Structure for storing decision data during parsing, these data are used for building nodes
-- in a decision tree
data TempTree = 
    TNode {
        tindex :: Int,
        tthreshold :: Double,
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



-- | Parses indentation
indentp :: (Default a) => Int -> ParserCtx a -> ParserCtx a
indentp 0 ctx = ctx
indentp n ctx = (replicate (2 * n) ' ' |! "indent") ctx


-- | Parses leaf
leafp :: Int -> ParserCtx BinaryDecisionTree -> ParserCtx BinaryDecisionTree
leafp lvl = (indentp lvl) +++ ("Leaf" |! "Leaf") +++ -- Parses indentation and Leaf keyword
    (<@) (":" |! ":") +++ (<@) (classnamep |> (\c _ -> TLeaf{tname=c})) -- ":" `classname`
    |> buildtree


-- | Parses node and its child
nodep :: Int -> ParserCtx BinaryDecisionTree -> ParserCtx BinaryDecisionTree
nodep lvl = (indentp lvl) +++ ("Node" |! "Node") +++ -- Parses indentation and Node keyword
    (<@) (":" |! ":") +++ (<@) (uintp |> (\i d -> d{tindex=i})) +++ -- ":" `feature_index`
    (<@) ("," |! ",") +++ (<@) (doublep |> (\t d -> d{tthreshold=t})) +++ -- "," `float_index`
    (<+.) ((<@) newlinep) +++
    (((nodep $ lvl + 1) <|> (leafp $ lvl + 1)) |> (\t d -> d{tleft=t})) +++ -- Left child
    (<+.) ((<@) newlinep) +++ 
    (((nodep $ lvl + 1) <|> (leafp $ lvl + 1)) |> (\t d -> d{tright=t})) -- Right child
    |> buildtree


-- | Parses a decision tree
treep :: ParserCtx BinaryDecisionTree -> ParserCtx BinaryDecisionTree
treep = ((nodep 0) <|> (leafp 0)) +++ (<*.) ((<@) newlinep)


-- | Wrapper above treep
parseTree :: String -> ParserResult BinaryDecisionTree
parseTree inputStr = finalize $ treep $ checkemptyinp $ initParserCtx inputStr


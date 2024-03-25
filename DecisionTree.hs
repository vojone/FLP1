{- | 
DecisionTree module
Module containing structure representing decision tree and its parts

Author: Vojtěch Dvořák (xdvora3o) 
-}

module DecisionTree
(
    BinaryTree(..),
    DecisionData(..),
    BinaryDecisionTree
) where


import Parser

-- | General structure for binary tree
data BinaryTree a =
    Empty | -- For special cases (empty file with tree etc.)
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


-- | Type for storing data that are necessary for making decision in a decision tree
data DecisionData = 
    Decision { index :: Int, threshold :: Float } |
    Class { name :: String }

instance Show DecisionData where
    show decData = case decData of
        (Decision {index=i, threshold=t}) -> show i ++ ", " ++ show t
        (Class {name=n}) -> n



-- | Specialized tree for binary deicision trees
type BinaryDecisionTree = BinaryTree DecisionData



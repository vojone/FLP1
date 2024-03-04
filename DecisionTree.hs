module DecisionTree
(
    BinaryTree,
    DecisionData
) where

import Data.Char
import Data.List

data BinaryTree =
    Node DecisionData BinaryTree BinaryTree |
    Leaf DecisionData
    deriving (Show)

data DecisionData = 
    Decision { index :: Int, threshold :: Float } |
    Class { name :: String }
    deriving (Show)




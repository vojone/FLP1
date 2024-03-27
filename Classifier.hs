{-
Classifier module

Used for classification of the data by the decision trees
Auhtor: Vojtěch Dvořák (xdvora3o) 
-}

module Classifier
(
    classify,
    classifyOne
) where

import DecisionTree
import MData


-- | Classifies the whole dataset (list of object)
classify :: BinaryTree DecisionData -> Dataset -> Dataset
classify tree = map (classifyOne tree)


-- | Classifies one object by traversing the decision tree
classifyOne :: BinaryTree DecisionData -> Object -> Object
classifyOne tree obj@(Object a Nothing) = case tree of
    (Leaf (Class className)) -> Object a (Just className)
    (Node (Decision i t) l r) | ((values a) !! i) < t -> classifyOne l obj
    (Node (Decision i t) l r) | ((values a) !! i) >= t -> classifyOne r obj
    _ -> obj
classifyOne _ obj@(Object _ _) = obj -- If the tree is empty or there is class already, do nothing


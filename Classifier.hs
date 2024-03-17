module Classifier
(
    classify,
    classifyOne
) where

import TreeParser
import DataParser

classify :: BinaryTree DecisionData -> Dataset Float -> Dataset Float
classify tree = map (classifyOne tree)

classifyOne :: BinaryTree DecisionData -> Object Float -> Object Float
classifyOne tree obj@(Object a Nothing) = case tree of
    (Leaf (Class className)) -> Object a (Just className)
    (Node (Decision i t) l r) | ((values a) !! i) < t -> classifyOne l obj
    (Node (Decision i t) l r) | ((values a) !! i) >= t -> classifyOne r obj
    _ -> obj
classifyOne _ obj@(Object _ _) = obj


module Classifier
(
    classify,
    classifyOne
) where

import TreeParser
import DataParser

classify :: BinaryDecisionTree -> [MData Float String] -> [MData Float String]
classify tree = map (classifyOne tree)

classifyOne :: BinaryDecisionTree -> MData Float String -> MData Float String
classifyOne tree mdata@(MData a Nothing) = case tree of
    (Leaf (Class className)) -> MData a (Just className)
    (Node (Decision i t) l r) | ((values a) !! i) < t -> classifyOne l mdata
    (Node (Decision i t) l r) | ((values a) !! i) >= t -> classifyOne r mdata
    _ -> mdata
classifyOne _ mdata@(MData _ _) = mdata


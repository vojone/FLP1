{-
Classifier module

Used for classification of the data by the decision trees
Auhtor: Vojtěch Dvořák (xdvora3o) 
-}

module Classifier
(
    ClassErr,
    ClassResult,
    DatasetClassResult,
    classify,
    classifyOne
) where

import DecisionTree
import MData


-- | The return type of classification
type ClassResult = Either ClassErr Object


-- | The return type of classification of the whole dataset
type DatasetClassResult = Either ClassErr Dataset

type ClassErr = String


-- | Classifies the whole dataset (list of object)
classify :: BinaryDecisionTree -> Dataset -> Either ClassErr Dataset
classify tree dset = fst $ foldr (classify' tree) (Right [], length dset) dset where
    classify' :: BinaryDecisionTree -> Object -> (DatasetClassResult, Int) -> (DatasetClassResult, Int)
    classify' tree' obj acc@(Left _, _) = acc
    classify' tree' obj (Right dset', i) = case classifyOne tree' obj of
        Left err -> (Left $ "Classification error at index " ++ show i ++ ": " ++ err, i)
        Right classObj -> (Right $ classObj:dset', i - 1)


-- | Makes decision in the tree due to the double data and double threshold
decideDouble :: Object -> (Double, Double) -> (BinaryDecisionTree, BinaryDecisionTree) -> ClassResult
decideDouble obj (val, t) (l, r) = if val < t then classifyOne l obj else classifyOne r obj


-- | Classifies one object by traversing the decision tree
classifyOne :: BinaryDecisionTree -> Object -> ClassResult
classifyOne tree obj@(Object _ Nothing) = case tree of
    (Leaf (Class className)) -> Right $ obj{cls=(Just className)}
    (Node (Decision i t) l r) -> case getAttr i obj of -- Try to get attribute of the object at index i
        None -> Left $ "Unable to find attr. with index " ++ show i
        someValue -> case unpackDouble someValue of -- Is this attribute double?
            Nothing -> Left $ "Bad type of attr. with index " ++ show i ++ ", expected double."
            Just doubleVal -> decideDouble obj (doubleVal, t) (l, r)
classifyOne _ obj@(Object _ _) = Right $ obj -- If the tree is empty or there is class already, do nothing


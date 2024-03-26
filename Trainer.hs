module Trainer (
    trainTree,
    getClasses
) where

import Data.Maybe
import Data.List

import DecisionTree
import CSVParser
import Utils

getClasses :: Dataset a -> [String]
getClasses = map getClass . filter (\(Object a c) -> isJust c) where
    getClass (Object a (Just c)) = c
    getClass (Object a _) = ""

getUniqueClasses :: Dataset a -> [String]
getUniqueClasses = nd . getClasses

cntClassMembers :: String -> Dataset a -> Int
cntClassMembers clsName = foldr (incCnter clsName) 0 where
    incCnter :: String -> Object a -> Int -> Int
    incCnter clsName  (Object _ cls) acc = case cls of
        Just c | c == clsName -> acc + 1
        _ -> acc

getMostProbableClass :: Dataset a -> String
getMostProbableClass objs = fst $ foldr (\c acc -> if fst acc == "" || snd acc < snd c then c else acc) ("", 0) $ map (\c -> (c, cntClassMembers c objs)) $ getUniqueClasses objs

getGini :: Dataset a -> Float
getGini objs = 1 - (sum $ sqProbs (memberCnts objs) (totalCnt objs)) where
    sqProbs :: [Int] -> Int -> [Float]
    sqProbs counts total = map (\c -> (c/(toFloat total))^2) (toFloats counts)
    memberCnts :: Dataset a -> [Int]
    memberCnts objs = map (\c -> cntClassMembers c objs) $ getUniqueClasses objs
    totalCnt :: Dataset a -> Int
    totalCnt = length

type DatasetSplit a = (Dataset a, Dataset a)

splitDataset :: (Ord a) => Dataset a -> Int -> a -> DatasetSplit a
splitDataset objs i thr = fst $ splitDatasetRec (([], []), objs) i thr where
    splitDatasetRec :: (Ord a) => (DatasetSplit a, Dataset a) -> Int -> a -> (DatasetSplit a, Dataset a)
    splitDatasetRec result@(_, []) _ _ = result
    splitDatasetRec ((l, r), o:objs) i thr = case o of
        Object (Attributes a) _ | (a !! i) < thr -> splitDatasetRec ((o:l, r), objs) i thr
        Object (Attributes a) _ | (a !! i) >= thr -> splitDatasetRec ((l, o:r), objs) i thr

getGiniOfSplit :: DatasetSplit a -> Float
getGiniOfSplit s@(d1, d2) = ( (toFloat $ length d1) / (toFloat $ totalCnt s))*getGini d1 + 
    ((toFloat $ length d2) /(toFloat $ totalCnt s))*getGini d2 where
    totalCnt :: DatasetSplit a -> Int
    totalCnt (d1, d2) = length d1 + length d2


findThreshold :: (Fractional a, Ord a) => Dataset a -> Int -> (a, DatasetSplit a, Float)
findThreshold objs i = foldr (\(t, s, g) (at, as, ag) -> if g < ag then (t, s, g) else (at, as, ag)) (select1 $ ((computeGinis objs i) !! 0), ([], []), 1.0) $ computeGinis objs i where
    computeGinis objs i = map (\(t, s) -> (t, s, getGiniOfSplit s)) $ createSplits objs i
    createSplits objs i = map (\t -> (t, splitDataset objs i t)) $ snd findPotentialThresholds 
    findPotentialThresholds = foldr (\x (it, l) -> if it > 0 then (it - 1, (avg x $ ln it):l) else (0, l)) (length vals - 1, []) vals
    avg :: (Fractional a, Ord a) => a -> a -> a
    avg x y = (x + y) / 2 
    vals = getAttrs i objs
    ln it = vals !! (it - 1)

getBestSplit :: (Fractional a, Ord a) => Dataset a -> (Int, a, DatasetSplit a)
getBestSplit objs = separateIndexThreshold $ getBestSplit' objs where
    separateIndexThreshold (i, (t, s, _)) = (i, t, s)
    getBestSplit' objs@((Object (Attributes a) _):_) = foldr (\(i, (t, s, g)) (ai, (at, as, ag)) -> if g < ag then (i, (t, s, g)) else (ai, (at, as, ag))) (0, (0.0, ([], []), 1.0)) $ computeGinis objs
    computeGinis objs@((Object (Attributes a) _):_) = map (\i -> (i, findThreshold objs i)) [0..length a - 1]


trainTree :: Dataset Float -> BinaryDecisionTree -> BinaryDecisionTree
trainTree [] _ = Empty
trainTree objs Empty = buildSubtree objs (getBestSplit objs) where
    buildSubtree :: Dataset Float -> (Int, Float, DatasetSplit Float) -> BinaryDecisionTree
    buildSubtree objs@(o:_) (_, _, ([], _)) = Leaf (Class $ getMostProbableClass objs)
    buildSubtree objs@(o:_) (_, _, (_, [])) = Leaf (Class $ getMostProbableClass objs)
    buildSubtree objs@(o:_) (index, threshold, (l, r)) = Node (Decision index threshold) (trainTree l Empty) (trainTree r Empty)
trainTree _ _ = Empty

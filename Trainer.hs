module Trainer (
    getClasses
) where

import Data.Maybe
import Data.List

import TreeParser
import DataParser
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
        Object (DataAttributes a) _ | (a !! i) < thr -> splitDatasetRec ((o:l, r), objs) i thr
        Object (DataAttributes a) _ | (a !! i) >= thr -> splitDatasetRec ((l, o:r), objs) i thr

getGiniOfSplit :: DatasetSplit a -> Float
getGiniOfSplit s@(d1, d2) = ( (toFloat $ length d1) / (toFloat $ totalCnt s))*getGini d1 + 
    ((toFloat $ length d2) /(toFloat $ totalCnt s))*getGini d2 where
    totalCnt :: DatasetSplit a -> Int
    totalCnt (d1, d2) = length d1 + length d2


findThreshold :: (Fractional a, Ord a) => Dataset a -> Int -> a
findThreshold objs i = fst $ foldr (\(t, g) (at, ag) -> if g < ag then (t, g) else (at, ag)) (fst ((computeGinis objs i) !! 0), 1.0) $ computeGinis objs i where
    computeGinis objs i = map (\(t, s) -> (t, getGiniOfSplit s)) $ createSplits objs i
    createSplits objs i = map (\t -> (t, splitDataset objs i t)) $ snd findPotentialThresholds 
    findPotentialThresholds = foldr (\x (it, l) -> if it > 0 then (it - 1, (avg x $ ln it):l) else (0, l)) (length vals - 1, []) vals
    avg :: (Fractional a, Ord a) => a -> a -> a
    avg x y = (x + y) / 2 
    vals = getFeatures i objs
    ln it = vals !! (it - 1)


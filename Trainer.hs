module Trainer (
    getClasses
) where

import Data.Maybe

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
    

-- iniIndex :: Float

-- train :: Dataset Float String -> BinaryDecisionTree
-- train :: 
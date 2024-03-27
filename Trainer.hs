{-
Trainer module
Module responsible of the traning of new trees

Author: Vojtěch Dvořák (xdvora3o)
-}

module Trainer (
    trainTree,
    getBestSplit,
) where


import MData
import DecisionTree

import Data.List
import Data.Maybe
import qualified Data.Set as Set(fromList, toList)


-- | Type for errors, that occur during the training
type TrainErr = String


-- | The type alias for better readability
type DatasetSplit = (Dataset, Dataset)


-- | No duplicates
nd :: (Ord a) => [a] -> [a]
nd = Set.toList . Set.fromList


-- | Converts integral type to double
toDouble :: (Integral a) => a -> Double
toDouble str = fromIntegral str :: Double


-- | Converts array of integral values to double values
toDoubles :: (Integral a) => [a] -> [Double]
toDoubles = map toDouble


-- | Gets the list with all classes of objects in the dataset
getClasses :: Dataset -> [String]
getClasses = map getClass . filter (\(Object _ c) -> isJust c) where
    getClass :: Object -> String
    getClass (Object _ (Just c)) = c
    getClass (Object _ _) = ""


-- | Removes multiple occurences of the same classes in the return value of getClasses
getUniqueClasses :: Dataset -> [String]
getUniqueClasses = nd . getClasses -- Recommended way how to implemented "no duplicate" in learnyouhaskell book


-- | Counts class members of the specific class
cntClassMembers :: String -> Dataset -> Int
cntClassMembers cname dset = length $ filter (hasClass cname) dset  where
    hasClass :: String -> Object -> Bool
    hasClass _ (Object _ Nothing) = False
    hasClass cname' (Object _ (Just c)) = cname' == c


-- | Returns list of tuples, where the first element is class that occurs in the dataset and the
-- second element of tuple is number of its members 
cntClassesMembers :: Dataset -> [(String, Int)]
cntClassesMembers dset = map (\c -> (c, cntClassMembers c dset)) $ getUniqueClasses dset


-- | Returns the name of the most probable class in the dataset
getMostProbableClass :: Dataset -> (String, Int)
getMostProbableClass dset = foldr storeBigger ("", 0) $ cntClassesMembers dset where
    storeBigger :: (String, Int) -> (String, Int) -> (String, Int)
    storeBigger n@(_, cnt) m@(_, cntm) = if cnt > cntm then n else m


-- | Computes gini index of the given dataset
getGini :: Dataset -> Double
getGini objs = 1 - (sum (sqProbs (classCnts objs) (totalCnt objs))) where
    sqProbs :: [Int] -> Int -> [Double]
    sqProbs counts total = map (\c -> (c / (toDouble total))^(2 :: Integer)) (toDoubles counts)
    classCnts :: Dataset -> [Int]
    classCnts dset = map (\(_, cnt) -> cnt) $ cntClassesMembers dset
    totalCnt :: Dataset -> Int
    totalCnt = length


-- | Result type of splitDatasetByDouble
type SplitRresult = Either TrainErr DatasetSplit


-- | Splits dataset by double feature at given index
splitDatasetByDouble :: Dataset -> Int -> Double -> SplitRresult
splitDatasetByDouble dset i thr = foldr (chooseDataset i thr) (Right $ ([], [])) dset where
    chooseDataset :: Int -> Double -> Object -> SplitRresult -> SplitRresult
    chooseDataset _ _ _ err@(Left _) = err
    chooseDataset i' thr' o@Object{attrs=(Attributes _)} (Right (dl, dr)) = case getAttr i' o of
        None -> Left $ "Train error: Unable to find attr. with index " ++ show i'
        VDouble v -> if v < thr' then (Right $ (o:dl, dr)) else (Right $ (dl, o:dr))
        _ -> Left $ "Train error: Bad type of attr. with index " ++ show i' ++ ", expected double."


-- | Computes gini index of given split
getGiniOfSplit :: DatasetSplit -> Double
getGiniOfSplit s@(dl, dr) = (getWeight dl s) * getGini dl + (getWeight dr s) * getGini dr where
    getWeight :: Dataset -> DatasetSplit -> Double
    getWeight dset dsplit = (toDouble (length dset)) / (toDouble (totalCnt dsplit))
    totalCnt :: DatasetSplit -> Int
    totalCnt (d1, d2) = length d1 + length d2


-- | Chooses the lower value of key, if there is a left value it is alwas returned
storeLower :: (Ord k) => Either a (k, v) -> Either a (k, v) -> Either a (k, v)
storeLower _ e@(Left _) = e
storeLower e@(Left _) _ = e
storeLower n@(Right (g, _)) m@(Right (mg, _)) = if g < mg then n else m


-- | Contains Gini of split, threshold and split itself
type SplitThresholdResult = Either TrainErr (Double, (Double, DatasetSplit))


-- | Find the best threshold for attribute with the given index
findBestDoubleThreshold :: Dataset -> Int -> SplitThresholdResult
findBestDoubleThreshold dset i = foldr storeLower initAcc $ findThresholds dset i where
    initAcc :: SplitThresholdResult
    initAcc = Right (2.0, (0.0, ([], [])))  -- Value 2 was chosen to be sure that every split will substite initial acc
    findThresholds :: Dataset -> Int -> [SplitThresholdResult]
    findThresholds dset' i' = map (createSplitThreshold) computeThresholds where
        computeThresholds :: [Double] -- Computes all potential thresholds (values in the middle of intervals)
        computeThresholds = [((x + y) / 2) | (x, y) <- zip getVals (tail getVals)]
        getVals :: [Double] -- Returns sorted array with values at index i'
        getVals = sort $ nd $ catMaybes $ map unpackDouble $ getAttrs i' dset'
        createSplitThreshold :: Double -> SplitThresholdResult
        createSplitThreshold thr = case splitDatasetByDouble dset' i' thr of
                Left err -> Left err
                Right s -> Right $ (getGiniOfSplit s, (thr, s))


-- | Return type of best split function
type BestSplitResult = Either TrainErr (Double, (Int, (Double, DatasetSplit)))


-- | Find the best split of the given dataset
getBestSplit :: Dataset -> BestSplitResult
getBestSplit dset = foldr storeLower (Right (2.0, (0, (0.0, ([], []))))) $ getSplits dset where
getSplits :: Dataset -> [BestSplitResult] -- Gets all candidate splits
getSplits [] = []
getSplits d@((Object (Attributes a) _):_) = map (getBestSplitAt d) [0..length a - 1] where
    getBestSplitAt :: Dataset -> Int -> BestSplitResult -- Computes the best threshold for the give nindex
    getBestSplitAt dset' i = case findBestDoubleThreshold dset' i of
        Left err -> Left err
        Right (g, (thr, s)) -> Right $ (g, (i, (thr, s)))


-- | Recursively builds a decision tree from the data
trainTree :: Dataset -> BinaryDecisionTree -> Either TrainErr BinaryDecisionTree
trainTree [] _ = Right $ Empty
trainTree dset Empty = buildSTree dset (getBestSplit dset) where
    buildSTree :: Dataset -> BestSplitResult -> Either TrainErr BinaryDecisionTree
    buildSTree _ (Left err) = Left $ err
    buildSTree d (Right (_, (_, (_, (_, []))))) = Right $ Leaf $ Class $ fst $ getMostProbableClass d -- If any part of the split is empty, choose the most probably class
    buildSTree d (Right (_, (_, (_, ([], _))))) = Right $ Leaf $ Class $ fst $ getMostProbableClass d
    buildSTree _ (Right (_, (i, (t, (l, r))))) = case (trainTree l Empty, trainTree r Empty) of
        (Left err, _) -> Left err
        (_, Left err) -> Left err
        (Right lchild, Right rchild) -> Right $ Node (Decision i t) lchild rchild -- Else build the node and its child recursively
trainTree _ _ = Right $ Empty

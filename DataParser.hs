module DataParser
(
    ClassData
) where

import Utils

data ClassData a b = ClassData {
    attributes :: [a],
    className :: b
} deriving Show


splitBy :: (Eq a) => [a] -> [a] -> [[a]]
splitBy delims str = fst $ splitByRec delims $ putToList ([], str) $ span (\x -> not $ x `elem` delims) str where
    putToList :: ([[a]], [a]) -> ([a], [a]) -> ([[a]], [a])
    putToList (l, _) (newstr, oldstr) = (append newstr l, oldstr)
    splitByRec :: (Eq a) => [a] -> ([[a]], [a]) -> ([[a]], [a])
    splitByRec delims res@(_, []) = res
    splitByRec delims res@(l, str) = splitByRec delims $ putToList res $ span (\x -> not $ x `elem` delims) $ removeSeperator str where
        removeSeperator [] = []
        removeSeperator res@(c:str) = if c `elem` delims then str else res

 
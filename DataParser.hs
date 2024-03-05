module DataParser
(
    MData(..),
    DataAttributes(..),
    parseUnclassifiedData,
    showClassName
) where

import Utils

data DataAttributes a = DataAttributes {
    values :: [a]
}

instance (Read a) => Read (DataAttributes a) where
    readsPrec _ str = [(DataAttributes (readValues str), "")] where
        readValues :: (Read a) => String -> [a]
        readValues str = map (read) $ split "," str

instance (Show a) => Show (DataAttributes a) where
    show (DataAttributes []) = ""
    show (DataAttributes (v:[])) = show v 
    show (DataAttributes (v:rem)) = show v ++ ", " ++ show (DataAttributes rem)


data MData a b = MData {
    attributes :: DataAttributes a,
    className :: Maybe b
} deriving Show


showClassName :: MData a String -> String
showClassName (MData _ c) = case c of
    Just cls -> id cls
    _ -> ""

-- showClassName :: (Show b) => MData a b -> String
-- showClassName (MData _ c) = case c of
--     Just cls -> show cls
--     _ -> ""




parseUnclassifiedData input = map toMData $ lines input where
    toMData line = MData (read line :: DataAttributes Float) (Nothing)

 
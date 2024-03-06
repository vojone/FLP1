module DataParser
(
    Object(..),
    Dataset(..),
    DataAttributes(..),
    parseUnclassifiedData,
    showStringClass,
    showStringClasses
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

data Object a b = Object {
    attributes :: DataAttributes a,
    mClass :: Maybe b
}

instance (Show a, Show b) => Show (Object a b) where
    show (Object a c) = show a ++ ", " ++ show c

type Dataset a b = [Object a b]

showStringClass :: Object a String -> String
showStringClass (Object _ c) = case c of
    Just cls -> id cls
    Nothing -> ""

showStringClasses :: Dataset a String -> String
showStringClasses objs = unlines $ map showStringClass objs


-- showClassName :: (Show b) => Object a b -> String
-- showClassName (Object _ c) = case c of
--     Just cls -> show cls
--     _ -> ""



parseUnclassifiedData :: (Read a) => String -> Dataset a b
parseUnclassifiedData input = map toObject $ lines input where
    toObject line = Object (read line) (Nothing)

 
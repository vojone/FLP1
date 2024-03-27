{-
MData module
Module containing structures that representing CSV-like data to be classified or used for training

Author: Vojtěch Dvořák
-}

module MData
(
    Val(..),
    unpackInt,
    unpackString,
    unpackDouble,
    Attributes(..),
    Object(..),
    Dataset,
    getAttr,
    getAttrs,
    showClasses,
    showDataset
) where


import Parser

-- | Structure for value of the object (value in one column of CSV)
data Val = 
    None |
    VInt Int |
    VString String |
    VDouble Double
    deriving (Ord, Eq) 

instance Show Val where
    show None = show ""
    show (VInt v) = show v
    show (VString v) = show v
    show (VDouble v) = show v


-- | Gets the raw integer
unpackInt :: Val -> Maybe Int
unpackInt (VInt v) = Just v
unpackInt _ = Nothing


-- | Gets the raw string
unpackString :: Val -> Maybe String
unpackString (VString v) = Just v
unpackString _ = Nothing


-- | Gets the raw string
unpackDouble :: Val -> Maybe Double
unpackDouble (VDouble v) = Just v
unpackDouble _ = Nothing


instance Default Val where
    defv = None


-- | Attributes of object (all values in the one row, expect of last if the data are classified)
data Attributes = Attributes {
    values :: [Val]
}

instance Show Attributes where
    show (Attributes []) = ""
    show (Attributes (v:[])) = show v 
    show (Attributes (v:rattrs)) = show v ++ ", " ++ show (Attributes rattrs)


-- | Object - all the data in one row in CSV
data Object = Object {
    attrs :: Attributes,
    cls :: Maybe String
}

instance Show Object where
    show Object{attrs=(Attributes []), cls=Nothing} = ""
    show Object{attrs=(Attributes []), cls=(Just c)} = c
    show Object{attrs=a@(Attributes _), cls=Nothing} = show a
    show Object{attrs=a@(Attributes _), cls=(Just c)} = show a ++ ", " ++ c 


instance Default Object where
    defv = Object (Attributes []) Nothing


-- | Dataset - represents the whole CSV table
type Dataset = [Object]


-- | Prints dataset in the most readable from (mainly for debuggin purposes)
showDataset :: Dataset -> String
showDataset dataset = unlines $ (map show dataset)


-- | Returns attribute value at given index
getAttr :: Int -> Object -> Val
getAttr index (Object (Attributes a) _) = if index >= length a || index < 0
    then None
    else a !! index


-- | Returns attribute values at given index of all objects in the dataset
getAttrs :: Int -> Dataset -> [Val]
getAttrs index objs = map (getAttr index) objs 


-- | Returns the class of the object, if object is unclassified, empty string is returned
showClass :: Object -> String
showClass (Object _ c) = case c of
    Just cname -> cname
    Nothing -> ""


-- | Returns atring with all of classes of all objects in the dataset
showClasses :: Dataset -> String
showClasses objs = unlines $ map showClass objs

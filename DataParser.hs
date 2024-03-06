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

data Object a = Object {
    attributes :: DataAttributes a,
    mClass :: Maybe String
}

instance (Read a) => Read (Object a) where
    readsPrec _ str = [(readObject $ split "," str, "")] where
        readObject :: (Read a) => [String] -> (Object a)
        readObject [] = Object (DataAttributes []) Nothing
        readObject (lastCol:[]) = (Object (DataAttributes [])) (Just lastCol)
        readObject (col:str) = prependAttr (read col) (readObject str) where
            prependAttr :: a -> (Object a) -> (Object a)
            prependAttr attr (Object (DataAttributes attrs) c) = Object (DataAttributes (attr:attrs)) c 

instance (Show a) => Show (Object a) where
    show o@(Object a c) = show a ++ ", " ++ showStringClass o

type Dataset a = [Object a]

showStringClass :: Object a -> String
showStringClass (Object _ c) = case c of
    Just cls -> id cls
    Nothing -> ""

showStringClasses :: Dataset a -> String
showStringClasses objs = unlines $ map showStringClass objs


-- showClassName :: (Show b) => Object a b -> String
-- showClassName (Object _ c) = case c of
--     Just cls -> show cls
--     _ -> ""



parseUnclassifiedData :: (Read a) => String -> Dataset a
parseUnclassifiedData input = map toObject $ lines input where
    toObject line = Object (read line) (Nothing)


parseClassifiedData :: (Read a) => String -> Dataset a
parseClassifiedData input = map toObject $ lines input where
    toObject line = Object (read line) (Nothing)

 
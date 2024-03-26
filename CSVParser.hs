module CSVParser
(
    Object(..),
    Dataset(..),
    Attributes(..),
    getAttr,
    getAttrs,
    showClasses,
    parseUnclassifiedData,
    parseClassifiedData
) where


import Utils
import Parser

import Data.Char

data Attributes a = Attributes {
    values :: [a]
}

instance (Show a) => Show (Attributes a) where
    show (Attributes []) = ""
    show (Attributes (v:[])) = show v 
    show (Attributes (v:rem)) = show v ++ ", " ++ show (Attributes rem)

data Object a = Object {
    attrs :: Attributes a,
    cls :: Maybe String
}

instance (Show a) => Show (Object a) where
    show Object{attrs=(Attributes []), cls=Nothing} = ""
    show Object{attrs=(Attributes []), cls=(Just c)} = c
    show Object{attrs=(Attributes a), cls=Nothing} = show a
    show Object{attrs=(Attributes a), cls=(Just c)} = show a ++ ", " ++ show c 


instance (Default a) => Default (Object a) where
    defv = Object (Attributes []) Nothing


type Dataset a = [Object a]

getAttr :: Int -> Object a -> a
getAttr index (Object (Attributes a) _) = (!!) a index


getAttrs :: Int -> Dataset a -> [a]
getAttrs index objs = map (getAttr index) objs 


showClass :: Object a -> String
showClass (Object _ c) = case c of
    Just cls -> id cls
    Nothing -> ""


showClasses :: Dataset a -> String
showClasses objs = unlines $ map showClass objs


addValue :: a -> Object a -> Object a
addValue f d@Object{attrs=(Attributes{values=v})} = d{attrs=(Attributes{values=(v ++ [f])})}


setClass :: String -> Object a -> Object a
setClass c d = d{cls=(Just c)}


addToDataset :: Object a -> Dataset a -> Dataset a
addToDataset o d = d ++ [o]  


-- | Parses float value
floatp :: ParserCtx Float -> ParserCtx Float
floatp = clearb +++ rule +++ convert read where
    rule :: ParserCtx Float -> ParserCtx Float
    rule = (<?.) ("-" |! "-") +++ (isDigit *|! "digit") +++ -- Integer part
        (<?.) (("." |! ".") +++ (isDigit *|! "digit")) -- Decimal (optional part)


-- | Parses classnames
classnamep :: ParserCtx String -> ParserCtx String
classnamep = clearb +++ ((not . isSpace) *|! "classname") +++ convert id


-- | Parses newline
newlinep :: (Default a) => ParserCtx a -> ParserCtx a
newlinep = ("\n" |! "newline") <|> ("\r\n" |! "")


classlessobjectp :: ParserCtx (Object Float) -> ParserCtx (Object Float)
classlessobjectp =
    (<@) (floatp |> addValue) +++ (<*.) ((<@) ("," |! ",") +++ ((<@) floatp |> addValue))


classlessdatasetp :: ParserCtx (Dataset Float) -> ParserCtx (Dataset Float)
classlessdatasetp = (<@) (classlessobjectp |> addToDataset) +++
    (<?.) ((<+.) ((<@) newlinep +++ (classlessobjectp |> addToDataset))) +++
    (<*.) ((<@) newlinep)



parseUnclassifiedData :: String -> ParserResult (Dataset Float)
parseUnclassifiedData inputStr = case classlessdatasetp $ initParserCtx inputStr of
    ParserCtx{pos=p,res=(Left (_, expects))} -> Left $ 
        ("Syntax error at " ++ show p ++ ": Expected one of " ++ show expects, expects)
    ParserCtx{res=r@(Right _)} -> r



objectp :: ParserCtx (Object Float) -> ParserCtx (Object Float)
objectp =
    (<@) (floatp |> addValue) +++ 
    (<+.) ((<@) ("," |! ",") +++ (<@) (classnamep |> setClass) +++ lahead ((<@) (newlinep <|> ("" |! ""))) <|>
    (<@) ("," |! ",") +++ ((<@) floatp |> addValue))



datasetp :: ParserCtx (Dataset Float) -> ParserCtx (Dataset Float)
datasetp = (<@) (objectp |> addToDataset) +++
    (<?.) ((<+.) ((<@) newlinep +++ (objectp |> addToDataset))) +++
    (<*.) ((<@) newlinep)


parseClassifiedData :: String -> ParserResult (Dataset Float)
parseClassifiedData inputStr = case datasetp $ initParserCtx inputStr of
    ParserCtx{pos=p,res=(Left (_, expects))} -> Left $ 
        ("Syntax error at " ++ show p ++ ": Expected one of " ++ show expects, expects)
    ParserCtx{res=r@(Right _)} -> r

 
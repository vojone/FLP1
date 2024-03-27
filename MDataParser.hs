{-
MDataParser module
Module responsible for parsing unclassified data (for classification) and also classified data (for
training)

Author: Vojtěch Dvořák (xdvora3o)
-}

module MDataParser
(
    parseUnclassifiedData,
    parseClassifiedData
) where


import Parser
import CommonParser
import MData

import Data.Char


-- | Adds value to the given object
addValue :: (a -> Val) -> a -> Object -> Object
addValue constr raw d@Object{attrs=(Attributes{values=v})} =
    d{attrs=(Attributes{values=(v ++ [constr raw])})}


-- | Sets class of the given object
setClass :: String -> Object -> Object
setClass c d = d{cls=(Just c)}


-- | Appends an object to the given dataset 
addToDataset :: Object -> Dataset -> Dataset
addToDataset o d = d ++ [o]


-- | Parses an unclassified object
classlessobjectp :: ParserCtx Object -> ParserCtx Object
classlessobjectp =
    (<@) (doublep |> (addValue VDouble)) +++ -- There should be at least one value
    (<*.) ((<@) ("," |! ",") +++ ((<@) doublep |> (addValue VDouble)))


-- | Parses dataset of unclassified objects
classlessdatasetp :: ParserCtx Dataset -> ParserCtx Dataset
classlessdatasetp = (<@) (classlessobjectp |> addToDataset) +++
    (<?.) ((<+.) ((<@) newlinep +++ (classlessobjectp |> addToDataset))) +++
    (<*.) ((<@) newlinep)


-- | Parses unclassified data
parseUnclassifiedData :: String -> ParserResult Dataset
parseUnclassifiedData inputStr = finalize $ classlessdatasetp $ initParserCtx inputStr


-- | Parses classified object
objectp :: ParserCtx Object -> ParserCtx Object
objectp =
    (<@) (doublep |> (addValue VDouble)) +++ -- There should be at least one value
    (<*.) (((<@) ("," |! ",") +++ ((<@) doublep |> (addValue VDouble)) +++
    lahead ((<@) ("," |! "")))) +++ -- lahead avoids the last column to be considered as some value
    (<@) ("," |! ",") +++ (<@) (classnamep |> setClass) <|> -- The Last column is class
    (<@) (classnamep |> setClass)


-- Parses dataset with classified objects
datasetp :: ParserCtx Dataset -> ParserCtx Dataset
datasetp = (<@) (objectp |> addToDataset) +++
    (<*.) ((<@) newlinep +++ (objectp |> addToDataset)) +++
    (<*.) ((<@) newlinep)


-- Parses classified data
parseClassifiedData :: String -> ParserResult Dataset
parseClassifiedData inputStr = finalize $ datasetp $ initParserCtx inputStr

 
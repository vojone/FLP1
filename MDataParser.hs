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
classlessheadobjectp :: ParserCtx Object -> ParserCtx Object
classlessheadobjectp =
    (<@) (doublep |> (addValue VDouble)) +++ -- There should be at least one value
    (<*.) ((<@) ("," |! ",") +++ ((<@) doublep |> (addValue VDouble)))


-- | Parses a value in one column in CSV also provides check that value type corresponds the
-- corresponding value in the reference object
columnvalp :: Object -> Int -> ParserCtx Object -> ParserCtx Object
columnvalp o i = case getAttr i o of
    None -> id
    VDouble _ -> (doublep |> (addValue VDouble))
    VInt _ -> (doublep |> (addValue VDouble))
    VString _ -> (stringp |> (addValue VString))


-- | Recursively parses columns in CSV except of the first one
columnsvalprec :: Object -> Int -> ParserCtx Object -> ParserCtx Object
columnsvalprec Object{attrs=(Attributes a)} i' | i' >= length a = id
columnsvalprec o i = (<@) ("," |! ",") +++ (<@) (columnvalp o i) +++ (columnsvalprec o (i + 1))


-- | Parses classified object
classlessobjectp :: Object -> ParserCtx Object -> ParserCtx Object
classlessobjectp refobj = (<@) (columnvalp refobj 0) +++ columnsvalprec refobj 1 where


-- | Parses dataset with unclassified objects
classlessdatasetp :: ParserCtx Dataset -> ParserCtx Dataset
classlessdatasetp ctx = case res $ ((<@) (classlessheadobjectp) |> addToDataset) ctx of
    Right [o] -> (((<@) (classlessheadobjectp) |> addToDataset) +++
        (<?.) ((<+.) ((<@) newlinep +++ ((classlessobjectp o) |> addToDataset))) +++
        (<*.) ((<@) newlinep)) ctx
    _ -> ((<@) (classlessheadobjectp) |> addToDataset) ctx


-- | Parses unclassified data
parseUnclassifiedData :: String -> ParserResult Dataset
parseUnclassifiedData inputStr = finalize $ classlessdatasetp $ checkemptyinp $ initParserCtx inputStr


-- | Parses classified object
objectp :: Object -> ParserCtx Object -> ParserCtx Object
objectp templateobj = (<@) (columnvalp templateobj 0) +++ columnsvalp templateobj 1 where
    columnsvalp :: Object -> Int -> ParserCtx Object -> ParserCtx Object
    columnsvalp o i = columnsvalprec o i +++
        (<@) ("," |! ",") +++ (<@) (classnamep |> setClass) -- The Last column is class


-- | Parses the first row in CSV with classified data
headobjectp :: ParserCtx Object -> ParserCtx Object
headobjectp =
    (<@) (doublep |> (addValue VDouble)) +++ -- There should be at least one value
    (<*.) (((<@) ("," |! ",") +++ ((<@) doublep |> (addValue VDouble)) +++
    lahead ((<@) ("," |! "")))) +++ -- lahead avoids the last column to be considered as some value
    (<@) ("," |! ",") +++ (<@) (classnamep |> setClass) <|> -- The Last column is class
    (<@) (classnamep |> setClass)


-- | Parses dataset with classified objects
datasetp :: ParserCtx Dataset -> ParserCtx Dataset
datasetp ctx = case res $ ((<@) (headobjectp) |> addToDataset) ctx of
    Right [o] -> (((<@) (headobjectp) |> addToDataset) +++
        (<*.) ((<+.) ((<@) newlinep) +++ ((objectp o) |> addToDataset)) +++
        (<*.) ((<@) newlinep)) ctx
    _ -> ((<@) (headobjectp) |> addToDataset) ctx


-- Parses classified data
parseClassifiedData :: String -> ParserResult Dataset
parseClassifiedData inputStr = finalize $ datasetp $ checkemptyinp $ initParserCtx inputStr

 
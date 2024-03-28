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
objectp :: Object -> ParserCtx Object -> ParserCtx Object
objectp templateobj = columnsvalp templateobj 0 where
    columnsvalp :: Object -> Int -> ParserCtx Object -> ParserCtx Object
    columnsvalp o i = (<@) (columnsvalprec o i) +++ (<@) (classnamep |> setClass) -- The Last column is class
    columnsvalprec :: Object -> Int -> ParserCtx Object -> ParserCtx Object
    columnsvalprec Object{attrs=(Attributes a)} i' | i' >= length a = id
    columnsvalprec o' i' = columnvalp o' i' +++ (<@) ("," |! ",") +++ (<@) (columnsvalprec o' (i' + 1))
    columnvalp :: Object -> Int -> ParserCtx Object -> ParserCtx Object
    columnvalp o' i' = case getAttr i' o' of
        None -> id
        VDouble _ -> (doublep |> (addValue VDouble))
        VInt _ -> (doublep |> (addValue VDouble))
        VString _ -> (stringp |> (addValue VString))


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
    Right [o] -> (((<@) (headobjectp) |> addToDataset) +++ (<*.) ((<+.) ((<@) newlinep) +++ ((objectp o) |> addToDataset)) +++ (<*.) ((<@) newlinep)) ctx
    _ -> ((<@) (headobjectp) |> addToDataset) ctx


-- Parses classified data
parseClassifiedData :: String -> ParserResult Dataset
parseClassifiedData inputStr = finalize $ datasetp $ initParserCtx inputStr

 
{- | 
ArgumentParser module
Module responsible for parsing of commandline arguments
Author: Vojtěch Dvořák (xdvora3o) 
-}

module ArgumentParser
(
    Task(..),
    Config(..),
    parseArgs,
) where

-- | Type for errors that occur during the parsing of arguments
type ArgErr = String


-- | Structure storing task and its variables, that can be configured by the user through
-- commandline arguments
data Task =
    Help | -- Defaul task (see configDefault)
    Classification String String |
    Training String
    deriving (Show, Eq)


-- | Structure for storing the result of the parsing, contains values for configurable variables
-- of the program
data Config = Config {
    programTask :: Task
} deriving (Show, Eq)



-- | Default configuration structure of program
configDefault :: Either ArgErr Config
configDefault = Right $ Config Help


-- | Argument parser return type, parsing can end with correct Config or with ArgErr
type ParserResult = Either ArgErr Config


-- | Type for argument hadnling function
-- Function takes existing config, list with the rest of arguments and returns
-- ParserResult (updated config or error) with thre rest of arguments in a tuple
type ArgumentParserF = Config -> [String] -> (ParserResult, [String])


-- | List of key-value pairs for each exclusive argument of program (only one
-- argument from this list can be passed)
exArgs :: [(String, ArgumentParserF)]
exArgs = [
--       arg    handler
        ("-1", parseClassificationArgs),
        ("-2", parseTrainingArgs)
    ]


-- | List of key-value pairs with nonexclusive arguments of the program
-- NOTE: Currently this list is unused, but it is integrated for better 
-- extensibility
nonexArgs :: [(String, a)]
nonexArgs = []


-- ------ Argument handlers -------

-- | Handling function for -1 (classification task)
-- -1 expects another two arguments - path to file with tree and path the file with data 
parseClassificationArgs :: ArgumentParserF
parseClassificationArgs _ [] = (Left "-1: missing paths to tree and data files! (see --help)", [])
parseClassificationArgs _ (_:[]) = (Left "-1: missing path to file with data! (see --help)", [])
parseClassificationArgs c (f1:f2:r) = (Right $ c {programTask=(Classification f1 f2)}, r)


-- | Handling function for -2 (training task)
-- -2 expects another one argument - path to the file with annotated data 
parseTrainingArgs :: ArgumentParserF
parseTrainingArgs _ [] = (Left "-2: missing path to the file with annot. data! (see --help)", [])
parseTrainingArgs c (f:r) = (Right $ c {programTask=(Training f)}, r)

-- --------------------------------



-- | Constructs readable string with all keys in the list with arguments, just for more readable
-- error messages
showKeys :: [(String, a)] -> String
showKeys argList = foldr catArgKeys "" $ map fst argList where
    catArgKeys :: String -> String -> String
    catArgKeys = \x acc -> if acc /= "" then "'" ++ x ++ "', " ++ acc else "'" ++ x ++ "'"


-- | Parses commandline arguments and its options, if argument has any
parseArg :: (ParserResult, [String]) -> (ParserResult, [String])
parseArg (res@(Right config), (arg:args)) = case (lookup arg exArgs, lookup arg nonexArgs) of
    (Just f, _) -> if res /= configDefault -- Argument is exclusive
        then (Left $ "Only one argument (one of " ++ (showKeys exArgs) ++ ") can be used!", [])
        else f config args
    (_, Just f) -> f config args -- Current argument is non-exclusive one
    (Nothing, Nothing) -> (Left $ "An unknown argument '" ++ arg ++ "'!", [])
parseArg x@((Right _), []) = x
parseArg x@((Left _), _) = x


-- | Parses a list with commandline arguments and creates Config structure base on this
-- array, if some error occurs during the argument parsing, ArgErr is returned instead of Config
parseArgs :: [String] -> ParserResult
parseArgs [] = configDefault
parseArgs args =
    let parseArgs' :: (ParserResult, [String]) -> (ParserResult, [String])
        parseArgs' x = parseArgs' $ parseArg x
    in if (elem "-h" args) || (elem "--help" args)
        then Right $ Config Help
        else fst $ parseArgs' (configDefault, args)


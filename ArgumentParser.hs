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

import Data.List

-- | Type for errors that occur during the parsing of arguments
type ArgErr = String

-- | Type for optional parameters of program or another argument
type Option a = Maybe a


-- | Structure storing task and its variables, that can be configured by the user through
-- commandline arguments
data Task =
    Help | -- Defaul task (see configDefault)
    Classification (Option String) (Option String) |
    Training (Option String)
    deriving (Show, Eq)


-- | Structure for storing the result of the parsing, contains values for configurable variables
-- of the program
data Config = Config {
    task :: Task
} deriving (Show, Eq)


-- | List of key-values pairs that contains all possible exclusive args (only one of them can be
-- passed to the commandline at the same time) 
exArgs :: [(String, Task)]
exArgs = [
        ("-1", Classification Nothing Nothing),
        ("-2", Training Nothing)
    ]

-- | List of key-values pairs with nonexclusive arguments of the program
nonexlusiveArgs :: [(String, a)]
nonexlusiveArgs = []


-- | Default configuration structure of program
configDefault :: Either ArgErr Config
configDefault = Right $ Config Help


-- | Provides check the task structure, if all mandatory parameters are spcified 
check :: Task -> Either ArgErr Task
check (Classification Nothing Nothing) = Left "-1 requires two arguments (see `--help` for usage)"
check (Classification _ Nothing) = Left "-1 requires two arguments (see `--help` for usage)"
check (Training Nothing) = Left "-2 requires one argument (see `--help` for usage)"
check t = Right t


-- | Adds paremeter to the given task
addParam :: Task -> String -> Either ArgErr Task
addParam (Classification Nothing Nothing) p = Right $ Classification (Just p) Nothing
addParam (Classification p1 Nothing) p = Right $ Classification p1 (Just p)
addParam (Training Nothing) p = Right $ Training (Just p)
addParam task _ = Right task


-- | Constructs readable string with all keys in the list with arguments 
showKeys :: [(String, a)] -> String
showKeys argList = foldr catArgKeys "" $ map fst argList where
    catArgKeys :: String -> String -> String
    catArgKeys = \x acc -> if acc /= "" then "'" ++ x ++ "', " ++ acc else "'" ++ x ++ "'"


-- | Parses options of commandline argument (repesented by task in this case)
parseParams :: (Either ArgErr Task, [String]) -> (Either ArgErr Task, [String])
parseParams ((Right task), []) = (check task, [])
parseParams (t@(Right task), orig@(a:args)) =
    let updatedTask = addParam task a :: Either ArgErr Task
    in if updatedTask == t
        then (Right task, orig)
        else parseParams (updatedTask, args)
parseParams x@(Left task, _) = x


-- | Parses commandline arguments and its options, if argument has any
parseArg :: (Either ArgErr Config, [String]) -> (Either ArgErr Config, [String])
parseArg (c@(Right _), (arg:args)) = case (lookup arg exArgs, lookup arg nonexlusiveArgs) of
    (Just task, _) -> if c /= configDefault -- Argument is exclusive
        then (Left $ "Only one argument (one of " ++ (showKeys exArgs) ++ ") can be used!", [])
        else createConfig task args
    (_, Just task) -> createConfig task args -- Current argument is non-exclusive one
    (Nothing, Nothing) -> (Left $ "An unexpected argument '" ++ arg ++ "'!", [])
    where
        -- | Creates config based on the Task structure (that represents an argument)
        createConfig :: Task -> [String] -> (Either ArgErr Config, [String])
        createConfig task args = case parseParams (Right task, args) of
            (Right t, rest) -> (Right $ Config t, rest)
            (Left err, rest) -> (Left err, [])
parseArg x@((Left _), args) = x


-- | Parses and array with commandline arguments and creates Config structure base on this
-- array, if some error occurs during the argument parsing, ArgErr is returned instead of Config
parseArgs :: [String] -> Either ArgErr Config
parseArgs [] = configDefault
parseArgs args =
    let parseArgs' :: (Either ArgErr Config, [String]) -> (Either ArgErr Config, [String])
        parseArgs' (config, []) = (config, [])
        parseArgs' x = parseArgs' $ parseArg x
    in if (elem "-h" args) || (elem "--help" args)
        then Right $ Config Help
        else fst $ parseArgs' (configDefault, args)


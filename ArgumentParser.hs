module ArgumentParser
(
    Task(..),
    Config(..),
    parseArgs,
) where


type ArgErr = String


type Option a = Maybe a


data Task =
    Help |
    Classification (Option String) (Option String) |
    Training (Option String)
    deriving (Show, Eq)


data Config = Config {
    task :: Task
} deriving (Show, Eq)


configDefault :: Either ArgErr Config
configDefault = Right $ Config Help


check :: Task -> Either ArgErr Task
check (Classification Nothing Nothing) = Left "-1 requires two arguments (see `--help` for usage)"
check (Classification _ Nothing) = Left "-1 requires two arguments (see `--help` for usage)"
check (Training Nothing) = Left "-2 requires two arguments (see `--help` for usage)"
check t = Right t


addParam :: Task -> String -> Task
addParam (Classification Nothing Nothing) p = Classification (Just p) Nothing
addParam (Classification p1 Nothing) p = Classification p1 (Just p)
addParam (Training Nothing) p = Training (Just p)
addParam task _ = task


parseParams :: (Task, [String]) -> (Either ArgErr Task, [String])
parseParams (task, []) = (check task, [])
parseParams (task, orig@(a:args)) =
    let updatedTask = addParam task a
    in if updatedTask == task
        then (Right task, orig)
        else parseParams (updatedTask, args)


parseArg :: (Either ArgErr Config, [String]) -> (Either ArgErr Config, [String])
parseArg ((Right c@(Config t)), (arg:args))
    | t /= Help = (Left $ "Only one option -1 or -2 can be specified!", [])
    | arg == "-1" = createConfig (Classification Nothing Nothing) args
    | arg == "-2" = createConfig (Training Nothing) args
    | otherwise = (Left $ "Unexpected argument " ++ arg ++ "!", [])
    where
        createConfig :: Task -> [String] -> (Either ArgErr Config, [String])
        createConfig task args = case parseParams (task, args) of
            (Right t, rest) -> (Right $ Config t, rest)
            (Left err, rest) -> (Left err, [])
parseArg x@((Left _), args) = x


parseArgs :: [String] -> Either ArgErr Config
parseArgs [] = configDefault
parseArgs args =
    let parseArgs' :: (Either ArgErr Config, [String]) -> (Either ArgErr Config, [String])
        parseArgs' (config, []) = (config, [])
        parseArgs' x = parseArgs' $ parseArg x
    in if (elem "-h" args) || (elem "--help" args)
        then Right $ Config Help
        else fst $ parseArgs' (configDefault, args)


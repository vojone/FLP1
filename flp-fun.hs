import System.Environment

data Task =
    Help |
    Classification !String !String |
    Training !String 
    deriving (Show, Eq)

data Config = Config {
    task :: Task
} deriving (Show)

configDefault :: Config
configDefault = Config Help

getFilePath :: [String] -> Int -> String
getFilePath list index = if index >= length list 
    then error "Missing filepath!"
    else list !! index 

shiftArgs :: [String] -> Int -> [String]
shiftArgs args 0 = args
shiftArgs [] _ = []
shiftArgs (_:args) i = shiftArgs args $ i - 1

parseArg :: Config -> String -> [String] -> (Config, [String])
parseArg (Config t) arg args
    | t /= Help = error "Usage error!"
    | arg == "-1" = (Config (Classification (getFilePath args 0) (getFilePath args 1)), shiftArgs args 2)
    | arg == "-2" = (Config (Training (getFilePath args 0)), shiftArgs args 1)
    | otherwise = error ("Unknown argument " ++ arg ++ "!")


parseArgs :: [String] -> Config
parseArgs [] = configDefault
parseArgs args =
    let parseArgs' :: Config -> [String] -> Config
        parseArgs' config [] = config
        parseArgs' config (arg:tail) = uncurry parseArgs' $ parseArg config arg tail
    in if (elem "-h" args) || (elem "--help" args)
        then Config Help
        else parseArgs' configDefault args



main :: IO ()
main = do
    args <- getArgs
    let config = parseArgs args
    putStrLn ("Config: " ++ (show config))
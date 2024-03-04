import System.Environment
import ArgumentParser
import TreeParser

main :: IO ()
main = do
    args <- getArgs
    let config = parseArgs args
    putStrLn ("Config: " ++ (show config))

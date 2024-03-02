import System.Environment
import ArgumentParser

main :: IO ()
main = do
    args <- getArgs
    let config = parseArgs args
    putStrLn ("Config: " ++ (show config))

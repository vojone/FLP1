import System.Environment
import System.IO
import ArgumentParser
import TreeParser


printHelp :: IO ()
printHelp = do
    putStrLn "Help TBD!"


classify :: String -> String -> IO ()
classify treeFilePath dataFilePath = do
    treeFileHandle <- openFile treeFilePath ReadMode
    treeFileHandleContents <- hGetContents treeFileHandle
    putStrLn $ show $ parse treeFileHandleContents

main :: IO ()
main = do
    args <- getArgs
    let config = parseArgs args
    case config of
        Config (Classification f1 f2) -> classify f1 f2
        _ -> printHelp
    

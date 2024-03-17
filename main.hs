import System.Environment
import System.IO
import ArgumentParser
import TreeParser
import DataParser
import Classifier
import Trainer


printHelp :: IO ()
printHelp = do
    putStrLn "Help TBD!"


classifyData :: String -> String -> IO ()
classifyData treeFilePath dataFilePath = do
    treeFileHandle <- openFile treeFilePath ReadMode
    treeFileContents <- hGetContents treeFileHandle
    dataFileHandle <- openFile dataFilePath ReadMode
    dataFileContents <- hGetContents dataFileHandle
    let tree = parse treeFileContents
    let newData = parseUnclassifiedData dataFileContents
    
    putStrLn $ showStringClasses $ classify tree newData

    hClose treeFileHandle
    hClose dataFileHandle


train :: String -> IO ()
train dataFilePath = do
    dataFileHandle <- openFile dataFilePath ReadMode
    dataFileContents <- hGetContents dataFileHandle
    let trainData = parseClassifiedData dataFileContents :: Dataset Float

    let tree = trainTree trainData Empty

    putStr $ show tree

    hClose dataFileHandle


main :: IO ()
main = do
    args <- getArgs
    let config = parseArgs args
    case config of
        Config (Classification f1 f2) -> classifyData f1 f2
        Config (Training f) -> train f
        _ -> printHelp
    

{-
Main.hs
The main body of flp-fun project

Author: Vojtěch Dvořák (xdvora3o)
-}

import System.Environment
import System.IO
import ArgumentParser
import BinaryDecisionTreeParser
import CSVParser
import Classifier
import Trainer
import Parser


printHelp :: IO ()
printHelp = do
    putStrLn "Help TBD!"


classifyData :: String -> String -> IO ()
classifyData treeFilePath dataFilePath = do
    treeFileHandle <- openFile treeFilePath ReadMode
    treeFileContents <- hGetContents treeFileHandle
    dataFileHandle <- openFile dataFilePath ReadMode
    dataFileContents <- hGetContents dataFileHandle
    let treeParseResult = parse treeFileContents

    case treeParseResult of
        Left err -> putStrLn $ fst err
        Right tree -> do
            let dataParseResult = parseUnclassifiedData dataFileContents
            case dataParseResult of
                Left err -> putStrLn $ fst err
                Right newdata -> putStrLn $ showClasses $ classify tree newdata

    hClose treeFileHandle
    hClose dataFileHandle


train :: String -> IO ()
train dataFilePath = do
    dataFileHandle <- openFile dataFilePath ReadMode
    dataFileContents <- hGetContents dataFileHandle
    -- let trainData = parseClassifiedData dataFileContents :: Dataset Float

    -- let tree = trainTree trainData Empty

    -- putStr $ show tree

    hClose dataFileHandle


-- | Main body of flp-fun program
main :: IO ()
main = do
    args <- getArgs
    let config = parseArgs args
    case config of
        Left err -> putStrLn $ "Usage error: " ++ err
        Right (Config (Classification treeFile dataFile)) -> classifyData treeFile dataFile
        Right (Config (Training dataFile)) -> train dataFile
        _ -> printHelp
    

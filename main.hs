{-
Main.hs
The main body of flp-fun project

Author: Vojtěch Dvořák (xdvora3o)
-}

import ArgumentParser
import DecisionTreeParser
import MDataParser
import MData
import Classifier
import Trainer

import System.Environment
import System.IO


-- | Prints help to stdout
printHelp :: IO ()
printHelp = do
    putStrLn "Help TBD!"


-- | Classifies data and prints classes to stdout
classifyData :: String -> String -> IO ()
classifyData treeFilePath dataFilePath = do

    -- Open files
    treeFileHandle <- openFile treeFilePath ReadMode
    treeFileContents <- hGetContents treeFileHandle
    dataFileHandle <- openFile dataFilePath ReadMode
    dataFileContents <- hGetContents dataFileHandle

    -- Make the parsing job
    let treeParseResult = parseTree treeFileContents
    let dataParseResult = parseUnclassifiedData dataFileContents

    -- Handle errors or prints the result if everything was OK
    case (treeParseResult, dataParseResult) of
        (Left err, _) -> putStrLn $ fst err
        (Right _, Left err) -> putStrLn $ fst err
        (Right tree, Right newdata) -> printClassResult $ classify tree newdata

    hClose treeFileHandle
    hClose dataFileHandle where
        printClassResult :: DatasetClassResult -> IO ()
        printClassResult result = case result of 
            Left err -> putStrLn err
            Right dset -> putStrLn $ showClasses dset 


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
    

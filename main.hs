{-
Main.hs
The main body of flp-fun project

Author: Vojtěch Dvořák (xdvora3o)
-}

import ArgumentParser
import DecisionTreeParser
import MData
import MDataParser
import DecisionTree
import Trainer
import Classifier

import System.Environment
import System.IO
import System.Exit


-- | Prints help to stdout
printHelp :: IO ()
printHelp = do
    putStrLn "flp-fun:"
    putStrLn "Tool for classification of data based on binary decision trees and training"
    putStrLn "new trees using annotated data"
    putStrLn ""
    putStrLn "USAGE:"
    putStrLn "./flp-fun {-1 <tree_path> <uncl_data_path>}|{-2 <classified data>}|{-h|--help}"
    putStrLn ""
    putStrLn "OPTIONS:"
    putStrLn "-1\tClassification - Classifies data in CSV format via given decision tree"
    putStrLn "-2\tTraining - Trains the new tree using the classified data in CSV format"
    putStrLn "-h|--help\tPrints help to stdout."
    putStrLn ""
    putStrLn "Results are printed to stdout."
    putStrLn ""
    putStrLn "RETURN:"
    putStrLn "1 if some error occured, otherwise 0.\n"



-- | Prints given message and exits with the specified code
putStrLnAndDie :: String -> Int -> IO ()
putStrLnAndDie err c = do
    putStrLn err
    exitWith (ExitFailure c)


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
        (Left err, _) -> putStrLnAndDie ("Tree parser: " ++ fst err) 1
        (Right _, Left err) -> putStrLnAndDie ("Data parser: " ++ fst err) 1
        (Right tree, Right newdata) -> printClassResult $ classify tree newdata

    hClose treeFileHandle
    hClose dataFileHandle where
        printClassResult :: DatasetClassResult -> IO ()
        printClassResult result = case result of 
            Left err -> putStrLnAndDie err 1
            Right dset -> putStrLn $ showClasses dset



-- | Trains the new decision tree on annotated dataset stored in the file
train :: String -> IO ()
train dataFilePath = do
    -- Open the file
    dataFileHandle <- openFile dataFilePath ReadMode
    dataFileContents <- hGetContents dataFileHandle
    
    -- Parse training data
    let dataParseResult = parseClassifiedData dataFileContents
    
    -- Process the result
    case dataParseResult of
        Left err -> putStrLnAndDie (fst err) 1
        -- Right trainData -> putStrLn $ showDataset trainData -- For debugging
        Right trainData -> do

            -- Do the training
            let trainResult = trainTree trainData Empty
            case trainResult of
                Left err -> putStrLnAndDie err 1
                Right tree -> putStr $ show tree

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
    

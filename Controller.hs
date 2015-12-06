module Controller where

import ReadFile
import CommandLine
import FCM

import System.IO

data IOArguments = IOArguments {
    inputFile :: String,
    outputFile  :: String
    } deriving (Show)

doTheThing :: CLA -> IO ()
doTheThing cla = 
    withFile fileName ReadMode $ \handle ->
    getParsedContent csvCLA handle >>= \fileData -> 
    calculateFCM fcmCLA fileData >>= \result ->
    optionalOutput output result where
        csvCLA = getParserArguments cla
        ioCLA = getIOArguments cla
        fcmCLA = getFCMArguments cla
        fileName = Controller.inputFile ioCLA
        output = Controller.outputFile ioCLA

optionalOutput :: (Show a) => String -> [[a]] -> IO ()
optionalOutput [] matrix = mapM_ print matrix
optionalOutput path matrix = writeFile path resultString where 
    resultString = getResultString matrix
    
getResultString :: (Show a) => [[a]] -> String
getResultString (r:[]) = show r ++ "\n"
getResultString (r:rs) = getResultString rs ++ getResultString [r]

getIOArguments :: CLA -> IOArguments
getIOArguments cla = IOArguments {
    Controller.inputFile = CommandLine.inputFile cla,
    Controller.outputFile = CommandLine.outputFile cla
    }
  
getParserArguments :: CLA -> ParserArguments
getParserArguments cla = ParserArguments { 
    ReadFile.getDelimeter = CommandLine.getDelimeter cla,
    ReadFile.getIgnoreHeader  = CommandLine.getIgnoreHeader cla,
    ReadFile.getIgnoreFirst  = CommandLine.getIgnoreFirst cla,
    ReadFile.getIgnoreLast  = CommandLine.getIgnoreLast cla
    }
    
getFCMArguments :: CLA -> FCMArguments
getFCMArguments cla = FCMArguments { 
    FCM.getClassCount = CommandLine.getClassCount cla,
    FCM.getAccuracy = CommandLine.getAccuracy cla,
    FCM.getMetrix = CommandLine.getMetrix cla,
    FCM.getInitialization = CommandLine.getInitialization cla
    }
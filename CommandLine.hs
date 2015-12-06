module CommandLine where

import ReadFile
import FCM

import Options.Applicative

data CLA = CLA {
    inputFile :: String,
    outputFile :: String,
    getClassCount :: Int,
    getAccuracy :: Double,
    getMetrix :: Metrix,
    getInitialization :: Initialization,
    getDelimeter  :: String,
    getIgnoreHeader  :: Bool,
    getIgnoreFirst  :: Bool,
    getIgnoreLast  :: Bool
    } deriving (Show)

constructDataFromCLA :: Parser CLA
constructDataFromCLA = CLA
        <$> argument str
            (  metavar "FILE"
            <> help "input file.")
        <*> option str
            (  long "output"
            <> short 'o'
            <> metavar "FILE"
            <> help "File with output results"
            <> value "")
        <*> option auto
            (  long "class-count"
            <> short 'c'
            <> metavar "COUNT"
            <> help "Class count")
        <*> option auto
            (  long "accuracy"
            <> short 'a'
            <> metavar "VALUE"
            <> help "Accuracy [0.0000001]"
            <> value 0.0000001 )
        <*> option auto
            (  long "metrix"
            <> short 'm'
            <> metavar "NAME"
            <> help "Dest [Euclid] | Hamming."
            <> value Euclid )
        <*> option auto
            (  long "initialization"
            <> short 'i'
            <> metavar "NAME"
            <> help "Initialization [Center] | Accessory."
            <> value Accessory)
        <*> option str
            (  long "delimiter"
            <> short 'd'
            <> metavar "VALUE"
            <> help "Delimiter for csv file. Default '.'"
            <> value ".")
        <*> switch
            (  long "ignore_header"
            <> short 'g'
            <> help "Skip header.")
        <*> switch
            (  long "ignore_first_element"
            <> short 'f'
            <> help "Skip first column.")
        <*> switch
            (  long "ignore_last_element"
            <> short 'l'
            <> help "Skip last column.")

getCLA :: ParserInfo CLA
getCLA = info (helper <*> constructDataFromCLA)
      ( fullDesc
     <> progDesc "Get parametrs for CSV"
     <> header "FCM application" )
 
parseCLA :: ParserInfo a -> IO a
parseCLA parserInfo = execParser parserInfo
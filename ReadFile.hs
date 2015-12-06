module ReadFile where

import System.IO
import Data.List.Split

data ParserArguments = ParserArguments {
    getDelimeter  :: String,
    getIgnoreHeader  :: Bool,
    getIgnoreFirst  :: Bool,
    getIgnoreLast  :: Bool
    } deriving (Show)

allElementsButLast :: [a] -> [a]
allElementsButLast (x:[]) = []
allElementsButLast (x:xs) = [x] ++ allElementsButLast xs
allElementsButLast _ = []

allElementsButFirstAndLast :: [a] -> [a]
allElementsButFirstAndLast list = (allElementsButLast . tail) list

skipHeader :: Bool -> String -> [String]
skipHeader True content = tail $ lines content
skipHeader False content = lines content

splitContentBySeparator :: String -> [String] -> [[String]]
splitContentBySeparator separator content = map (splitOn separator) content

getSplitedReducedContent :: Bool -> Bool -> String -> [String] -> [[String]]
getSplitedReducedContent True True separator content = map allElementsButFirstAndLast $ splitContentBySeparator separator content
getSplitedReducedContent True False separator content = map tail $ splitContentBySeparator separator content
getSplitedReducedContent False True separator content = map allElementsButLast $ splitContentBySeparator separator content
getSplitedReducedContent False False separator content = splitContentBySeparator separator content

getParsedContent :: ParserArguments -> Handle -> IO [[String]]
getParsedContent cla handle = 
  hGetContents handle >>= \content -> 
    let ignoreLast = getIgnoreLast(cla) in
    let ignoreFirst = getIgnoreFirst(cla) in 
    let ignoreHeader = getIgnoreHeader(cla) in
    let delimeter = getDelimeter(cla) in
    let contentWithoutHeader = skipHeader ignoreHeader content in
    let splitedcontentWithoutHeader = getSplitedReducedContent ignoreFirst ignoreLast delimeter contentWithoutHeader in
  return splitedcontentWithoutHeader
 
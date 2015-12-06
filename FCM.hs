module FCM where

import qualified Data.Vector.Unboxed as U
import System.Random.Mersenne
import Control.Monad

data Metrix = Euclid | Hamming deriving(Show, Read)
data Initialization = Center | Accessory deriving(Show, Read)

type SuppliesMatrix = [SuppliesVector]
type SuppliesVector = [Double]
type ClassCenter = [Double]
type FeatureVector = [Double]
type ProbVector = [Double]

data FCMArguments = FCMArguments {
    getClassCount :: Int,
    getAccuracy :: Double,
    getMetrix :: Metrix,
    getInitialization :: Initialization
    } deriving (Show)

------------------------------------------------------------------
matrixOk :: Double -> SuppliesMatrix -> SuppliesMatrix -> Bool
matrixOk accuracy m_x m_y = val <= accuracy where
    val = matrixNorm m_x m_y

matrixNorm :: SuppliesMatrix -> SuppliesMatrix -> Double
matrixNorm m_x m_y = maximum absDiffVec where
    absDiffVec = map (abs) diffVec
    diffVec = zipWith (-) v_x v_y
    v_x = matrixToVector m_x
    v_y = matrixToVector m_y

dest :: Metrix -> FeatureVector -> ClassCenter -> Double
dest Euclid f_vector c_center = euclidDest f_vector c_center
dest Hamming f_vector c_center = hammingDest f_vector c_center

hammingDest :: FeatureVector -> ClassCenter -> Double
hammingDest f_vector c_center = sum absDiffVec where
    diffVec = zipWith (-) f_vector c_center
    absDiffVec = map (abs) diffVec
    
euclidDest :: FeatureVector -> ClassCenter -> Double
euclidDest f_vector c_center = sqrt $ sum sqrDiffVec where
    diffVec = zipWith (-) f_vector c_center
    sqrDiffVec = map (**2) diffVec
------------------------------------------------------------------

------------------------------------------------------------------
getFeatureVectors :: [[String]] -> IO [FeatureVector]
getFeatureVectors content = return [map (read) elem | elem <- content] 

normMatrix :: SuppliesMatrix -> SuppliesMatrix
normMatrix matrix = map normList matrix

normList :: Fractional a => [a] -> [a]
normList list = map (/total) list where
    total = sum list
    
matrixToVector :: SuppliesMatrix -> SuppliesVector
matrixToVector (x:[]) = x
matrixToVector (x:xs) = x ++ matrixToVector xs

vectorToMatrix :: Int -> Int -> SuppliesVector -> SuppliesMatrix
vectorToMatrix 0 cols vector = []
vectorToMatrix rows cols vector = [take cols vector] ++ vectorToMatrix newRows cols newVector where
    newRows = rows - 1
    newVector = drop cols vector

fVectorLength :: [FeatureVector] -> Int
fVectorLength (x:xs) = length x
    
getStartMatrixOrClasses :: Initialization -> FCMArguments -> [FeatureVector] -> IO SuppliesMatrix
getStartMatrixOrClasses Accessory fcmArgs f_vectors = getStartMatrix (length f_vectors) count where
    count = FCM.getClassCount fcmArgs
getStartMatrixOrClasses Center fcmArgs f_vectors = 
    getStartMatrix count length >>= \classCenters ->
    return $ suppliesMatrix metrix f_vectors classCenters where
    count = FCM.getClassCount fcmArgs
    metrix = FCM.getMetrix fcmArgs
    length = fVectorLength  f_vectors

getStartMatrix :: Int -> Int -> IO SuppliesMatrix
getStartMatrix rows cols = 
    newMTGen Nothing >>= \source ->
    randoms source >>= \rs ->
    let count = rows*cols in
    let a = U.fromList (take count rs) :: U.Vector Double in
    let vector = U.toList a in
    let matrix = vectorToMatrix rows cols vector in
    let normedMatrix = normMatrix matrix in
    return $ normedMatrix
    
------------------------------------------------------------------
calculateFCM :: FCMArguments -> [[String]] -> IO [[Double]]
calculateFCM fcmArgs fileData =
    getFeatureVectors fileData >>= \featureVectors ->
    getStartMatrixOrClasses init fcmArgs featureVectors >>= \startMatrix ->
    return $ iteration fcmArgs startMatrix featureVectors where
    count = FCM.getClassCount fcmArgs
    init = FCM.getInitialization fcmArgs
    
iteration :: FCMArguments -> SuppliesMatrix -> [FeatureVector] -> SuppliesMatrix
iteration fcmArgs s_matrix f_vectors = iterationResult maybeOk fcmArgs newSuppliesMatrix f_vectors where
    classCenters = getClassCenters s_matrix f_vectors
    newSuppliesMatrix = suppliesMatrix metrix f_vectors classCenters
    maybeOk = matrixOk accuracy newSuppliesMatrix s_matrix
    accuracy = FCM.getAccuracy fcmArgs
    metrix = FCM.getMetrix fcmArgs
    
iterationResult :: Bool -> FCMArguments -> SuppliesMatrix -> [FeatureVector] -> SuppliesMatrix
iterationResult False fcmArgs s_matrix f_vectors = iteration fcmArgs s_matrix f_vectors
iterationResult True _ s_matrix _ = s_matrix

------------------------------------------------------------------    
getElemByPos :: Int -> [a] -> a
getElemByPos pos list = list !! (pos - 1)

getProbVector :: Int -> SuppliesMatrix -> ProbVector
getProbVector pos s_matrix = map (getElemByPos pos) s_matrix

sumVector :: ProbVector -> [FeatureVector] -> [Double]
sumVector (prob:[]) (f_vector:[]) = map (*prob) f_vector
sumVector (prob:probs) (f_vector:f_vectors) = zipWith (+) currentVector tailVectors where
    currentVector = sumVector [prob] [f_vector]
    tailVectors = sumVector probs f_vectors

classCenter :: ProbVector -> [FeatureVector] -> ClassCenter
classCenter p_vector f_vectors = map (/sumProb) sumVec where
    poweredVector = map (**weigth) p_vector
    sumVec = sumVector poweredVector f_vectors
    sumProb = sum poweredVector
    weigth = 2
    
getClassCenters :: SuppliesMatrix -> [FeatureVector] -> [ClassCenter]
getClassCenters ([]:xs) _ = []
getClassCenters s_matrix f_vectors = [nextClassCenter] ++ tailCenters where
    nextClassCenter = classCenter probVector f_vectors
    probVector = getProbVector 1 s_matrix
    tailCenters = getClassCenters tailProbs f_vectors
    tailProbs = map (tail) s_matrix
------------------------------------------------------------------

destValue :: Metrix -> FeatureVector -> ClassCenter -> ClassCenter -> Double
destValue metrix f_vector const_center curr_center = destRatio ** (2 / (weight - 1)) where
    destRatio = constDest / currDest
    constDest = destToVector const_center
    currDest = destToVector curr_center
    destToVector = dest metrix f_vector
    weight = 2

suppliesMatrixElem :: Metrix -> FeatureVector -> [ClassCenter] -> ClassCenter -> Double
suppliesMatrixElem metrix f_vector c_centers center = destSum ** (-1) where
    destSum = sum destVector
    destVector = map (destValue metrix f_vector center) c_centers

suppliesMatrixRow :: Metrix -> FeatureVector -> [ClassCenter] -> [ClassCenter] -> SuppliesVector
suppliesMatrixRow _ _ ([]) _ = [] 
suppliesMatrixRow metrix f_vector (center:cx) c_centers = [nextRowElement] ++ tailRowElements where
    nextRowElement = suppliesMatrixElem metrix f_vector c_centers center
    tailRowElements = suppliesMatrixRow metrix f_vector cx c_centers

suppliesMatrix :: Metrix -> [FeatureVector] -> [ClassCenter] -> SuppliesMatrix
suppliesMatrix _ [] _ = []
suppliesMatrix metrix (f_vector:f_tail) c_centers = [nextMatrixRow] ++ tailRows where
    nextMatrixRow = suppliesMatrixRow metrix f_vector c_centers c_centers
    tailRows = suppliesMatrix metrix f_tail c_centers
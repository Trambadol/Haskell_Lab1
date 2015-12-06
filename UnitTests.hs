module Tests where

import FCM
import Test.HUnit

main :: IO()
main =
    let hamTest1 = TestCase (assertEqual "for (hammingDest [0.0,0.0] [0.0,0.0])" 0.0 (hammingDest [0.0,0.0] [0.0,0.0])) in 
    let hamTest2 = TestCase (assertEqual "for (hammingDest [1.0,2.0] [3.0,4.0])" 4.0 (hammingDest [1.0,2.0] [3.0,4.0])) in
    let eucTest1 = TestCase (assertEqual "for (euclidDest [0.0,0.0] [0.0,0.0])" 0.0 (euclidDest [0.0,0.0] [0.0,0.0])) in
    let eucTest2 = TestCase (assertEqual "for (euclidDest [2.0,2.0,2.0,2.0] [4.0,4.0,4.0,4.0])" 4.0 (euclidDest [2.0,2.0,2.0,2.0] [4.0,4.0,4.0,4.0])) in 
    let normTest1 = TestCase (assertEqual "for (matrixNorm [[0.0,0.0],[0.0,0.0]] [[0.0,0.0],[0.0,0.0]])" 0.0 (matrixNorm  [[0.0,0.0],[0.0,0.0]] [[0.0,0.0],[0.0,0.0]])) in
    let normTest2 = TestCase (assertEqual "for (matrixNorm [[1.0,2.0],[3.0,4.0]] [[5.0,6.0],[7.0,8.0]])" 4.0 (matrixNorm [[1.0,2.0],[3.0,4.0]] [[5.0,6.0],[7.0,8.0]])) in
    let tests = TestList [hamTest1, hamTest2, eucTest1, eucTest2, normTest1, normTest2] in
    runTestTT tests >>= \counts ->
    return ()
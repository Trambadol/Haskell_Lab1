module Main where 

import ReadFile
import CommandLine
import Handler
import Controller

import Control.Exception
import System.IO

main :: IO ()
main =
  parseCLA getCLA >>= \cla -> 
  catch (doTheThing cla) handler
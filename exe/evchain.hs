module Main where

import System.Console.CmdArgs

import HEP.Automation.MadGraph.EventChain.ProgType
import HEP.Automation.MadGraph.EventChain.Command

main :: IO () 
main = do 
  putStrLn "evchain"
  param <- cmdArgs mode

  commandLineProcess param
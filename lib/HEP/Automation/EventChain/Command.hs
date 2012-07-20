module HEP.Automation.MadGraph.EventChain.Command where

import HEP.Automation.MadGraph.EventChain.ProgType
import HEP.Automation.MadGraph.EventChain.Job

commandLineProcess :: Evchain -> IO ()
commandLineProcess Test = do 
  putStrLn "test called"
  startJob

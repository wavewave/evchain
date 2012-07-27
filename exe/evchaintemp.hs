{-# LANGUAGE ScopedTypeVariables, RecordWildCards #-}

module Main where

import           Control.Monad 
import qualified Data.HashMap.Lazy as HM

import HEP.Automation.EventChain.Type.Spec
import HEP.Automation.EventChain.SpecDSL
import HEP.Automation.EventChain.Simulator 
import HEP.Automation.EventChain.Process



spec_testD = d ([1,2,3],[1,2,d ([3,9,10],[2,2])])

spec_testX = x (t proton,t proton,[spec_testD,1000021,1000021])

spec_sitestX = mkSICross spec_testX 

spec_sitestD = mkSIDecay 1 spec_testD

main = do 

  rm <- createProcessX genX genD countProcess spec_sitestX 100 
  print (HM.map (length.events) rm)
  case HM.lookup [] rm of 
    Nothing -> error "hey"
    Just proc -> do let c = countProcess proc
                        nlst = mkOccNum 1 (incounter c) 
                    print c 
                    print nlst 
                 



genX c n = replicateM n (generateEventSX c) >>= return . DummyProcess 

genD d n = replicateM n (generateEventSD d) >>= return . DummyProcess
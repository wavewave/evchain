{-# LANGUAGE ScopedTypeVariables, RecordWildCards #-}

module Main where

import qualified Data.HashMap.Lazy as HM

import HEP.Automation.EventChain.Type.Spec
import HEP.Automation.EventChain.SpecDSL
import HEP.Automation.EventChain.Simulator 




spec_testD = d ([1,2,3],[1,2,d ([3,9,10],[2,2])])

spec_testX = x (t proton,t proton,[spec_testD,1000021,1000021])

spec_sitestX = mkSICross spec_testX 

spec_sitestD = mkSIDecay 1 spec_testD

main = do 
  -- r <- generateEventSX spec_sitest2
  {-
  rm <- createProcessD spec_sitestD [] HM.empty [(1,10),(2,5),(3,5)]

  let rm2 = HM.map (length.events) rm 
  print rm2
  -}

  rm <- createProcessX spec_sitestX 100 
  print (HM.map (length.events) rm)
  case HM.lookup [] rm of 
    Nothing -> error "hey"
    Just proc -> do let c = countProcess proc
                        nlst = mkOccNum 1 (incounter c) 
                    print c 
                    print nlst 
                 

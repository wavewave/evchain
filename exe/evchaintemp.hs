{-# LANGUAGE ScopedTypeVariables, RecordWildCards #-}

module Main where

import           Control.Monad 
import qualified Data.HashMap.Lazy as HM

import HEP.Automation.EventChain.Type.Spec
import HEP.Automation.EventChain.SpecDSL
import HEP.Automation.EventChain.Simulator 
import HEP.Automation.EventChain.Process
import HEP.Automation.EventChain.Process.Generator


spec_testD = dc ("hello",[1,2,3],[1,2,dc ("test2",[3,9,10],[2,2])])

spec_testX = xc ( "\ngenerate P P > t t~ QED=99\n"
                , t proton, t proton
                , [6,-6] )

spec_sitestX = mkSICross spec_testX 

spec_sitestD = mkSIDecay 1 spec_testD

main = do 

  rm <- createProcessX testmadgraphX testmadgraphD lheCounter spec_sitestX 100 
  print rm 
  -- print (HM.map (length.events) rm)
{-
  case HM.lookup [] rm of 
    Nothing -> error "hey"
    Just proc -> do let c = countProcess proc
                        nlst = mkOccNum 1 (incounter c) 
                    print c 
                    print nlst 
                 
-}


{-# LANGUAGE ScopedTypeVariables, RecordWildCards #-}

module Main where

import           Control.Monad 
import qualified Data.HashMap.Lazy as HM

import HEP.Automation.EventChain.Type.Skeleton
import HEP.Automation.EventChain.Type.Spec
import HEP.Automation.EventChain.SpecDSL
import HEP.Automation.EventChain.Simulator 
import HEP.Automation.EventChain.Process
import HEP.Automation.EventChain.Process.Generator

{-
spec_testD = dc ("hello",[1,2,3],[1,2,dc ("test2",[3,9,10],[2,2])])

spec_testX = xc ( "\ngenerate P P > t t~ QED=99\n"
                , t proton, t proton
                , [6,-6] )

spec_sitestX = mkSICross spec_testX 

spec_sitestD = mkSIDecay 1 spec_testD
-}


spec_testX = MkC "\ngenerate P P > t t~ QED=99\n" 
                 (MkT (1,proton), MkT (2,proton)) 
                 [MkT (3,[6]), MkT (4,[-6])]



spec_ttbar = x (t proton,t proton, [spec_topdecay, spec_antitopdecay]) 

spec_topdecay = d ([6], [24,5] ) 

spec_antitopdecay = d ([-6], [-24,-5])


spec_ttbar_idx = mkCrossIDIdx (mkDICross spec_ttbar )


main = do 
  print spec_ttbar_idx
{-
  rm <- createProcessX testmadgraphX testmadgraphD (lheCounter spec_testX) spec_testX 100 
  print rm 
  -- print (HM.map (length.events) rm)
-}



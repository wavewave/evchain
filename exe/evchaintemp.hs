{-# LANGUAGE ScopedTypeVariables, NoMonomorphismRestriction, RecordWildCards #-}

module Main where

import           Control.Monad 
import qualified Data.HashMap.Lazy as HM

import HEP.Automation.EventChain.Type.Skeleton
import HEP.Automation.EventChain.Type.Spec
import HEP.Automation.EventChain.Type.Process
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



-- spec_ttbar = x (t proton,t proton, [spec_topdecay, spec_antitopdecay]) 
-- spec_topdecay = d ([6], [24,5] ) 
-- spec_antitopdecay = d ([-6], [-24,-5])

-- spec_ttbar_idx = mkCrossIDIdx (mkDICross spec_ttbar )

{-
pmap :: ProcSpecMap
pmap = HM.fromList [(Nothing,"\ngenerate P P > t t~ QED=99\n")
                   ,(Just (3,6,[]), "\ngenerate t > w+ b QED=99\n" )
                   ,(Just (4,-6,[]), "\ngenerate t~ > w- b~ QED=99\n")
                   ] 
-}

{-
spec2_tbbar = x (t proton,t proton, [spec2_topdecay, t [5,-5]]) 
spec2_topdecay = d ([6,-6], [t [24,-24],t [5,-5]] ) 
-- spec2_antitopdecay = d ([-6], [-24,-5])

spec2_tbbar_idx = mkCrossIDIdx (mkDICross spec2_tbbar )


pmap2 :: ProcSpecMap
pmap2 = HM.fromList [(Nothing,"\ngenerate P P > t b~ QED=99\nadd process P P > t~ b QED=99\n")
                    ,(Just (3,6,[]), "\ngenerate t > w+ b QED=99\n" )
                    ,(Just (3,-6,[]), "\ngenerate t~ > w- b~ QED=99\n")
                    ] 
-}


spec3_tbbar = x (t proton,t proton, [spec3_topdecay, t [5,-5]]) 
spec3_topdecay = d ([6,-6], [spec3_wdecay, t [5,-5]] ) 
-- spec2_antitopdecay = d ([-6], [-24,-5])

spec3_wdecay = d ([24,-24], [t [11,-11], t [12,-12]])

spec3_tbbar_idx = mkCrossIDIdx (mkDICross spec3_tbbar )


pmap3 :: ProcSpecMap
pmap3 = HM.fromList [(Nothing,"\ngenerate P P > t b~ QED=99\nadd process P P > t~ b QED=99\n")
                    ,(Just (3,6,[]), "\ngenerate t > w+ b QED=99\n" )
                    ,(Just (3,-6,[]), "\ngenerate t~ > w- b~ QED=99\n")
                    ,(Just (1,24,[3]),"\ngenerate w+ > e+ ve QED=99\n")
                    ,(Just (1,-24,[3]),"\ngenerate w- > e- ve~ QED=99\n")
                    ] 



main = do 
  print spec3_tbbar_idx

  rm <- createProcessX (generateX pmap3) (generateD pmap3) 
          lheCntX lheCntD spec3_tbbar_idx 100 
  print rm 
  -- print (HM.map (length.events) rm)




{-# LANGUAGE ScopedTypeVariables, NoMonomorphismRestriction, RecordWildCards #-}

module Main where

import           Control.Applicative
import           Control.Monad 
import           Control.Monad.Error 
import           Control.Monad.State 
import           Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Traversable as T
import qualified Data.HashMap.Lazy as HM
import           Data.Maybe 
import           System.IO
-- 
import HEP.Parser.LHEParser.Type
-- 
import HEP.Automation.EventChain.LHEConn
import HEP.Automation.EventChain.FileDriver
import HEP.Automation.EventChain.Type.Skeleton
import HEP.Automation.EventChain.Type.Spec
import HEP.Automation.EventChain.Type.Process
import HEP.Automation.EventChain.SpecDSL
import HEP.Automation.EventChain.Simulator 
import HEP.Automation.EventChain.Process
import HEP.Automation.EventChain.Process.Generator
import HEP.Automation.EventChain.Main 


{-
spec_testD = dc ("hello",[1,2,3],[1,2,dc ("test2",[3,9,10],[2,2])])

spec_testX = xc ( "\ngenerate P P > t t~ QED=99\n"
                , t proton, t proton
                , [6,-6] )

spec_sitestX = mkSICross spec_testX 

spec_sitestD = mkSIDecay 1 spec_testD
-}

{- 
spec_testX = MkC "\ngenerate P P > t t~ QED=99\n" 
                 (MkT (1,proton), MkT (2,proton)) 
                 [MkT (3,[6]), MkT (4,[-6])]
-}


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

{-
spec3_tbbar :: DCross  
spec3_tbbar = x (t proton,t proton, [spec3_topdecay, t [5,-5]]) 

spec3_topdecay :: DDecay 
spec3_topdecay = d ([6,-6], [spec3_wdecay, t [5,-5]] ) 
-- spec2_antitopdecay = d ([-6], [-24,-5])

spec3_wdecay :: DDecay 
spec3_wdecay = d ([24,-24], [t [11,-11], t [12,-12]])

spec3_tbbar_idx :: CrossID ProcSmplIdx
spec3_tbbar_idx = mkCrossIDIdx (mkDICross spec3_tbbar )


pmap3 :: ProcSpecMap
pmap3 = HM.fromList [(Nothing,"\ngenerate P P > t b~ QED=99\nadd process P P > t~ b QED=99\n")
                    ,(Just (3,6,[]), "\ngenerate t > w+ b QED=99\n" )
                    ,(Just (3,-6,[]), "\ngenerate t~ > w- b~ QED=99\n")
                    ,(Just (1,24,[3]),"\ngenerate w+ > e+ ve QED=99\n")
                    ,(Just (1,-24,[3]),"\ngenerate w- > e- ve~ QED=99\n")
                    ] 

-}

jets = [1,2,3,4,-1,-2,-3,-4,21]

adms = [9000201,-9000201,9000202,-9000202]

p_multijet :: DCross  
p_multijet = x (t proton,t proton, [p_go, p_go]) 

p_go :: DDecay 
p_go = d ([1000021], [p_neut, t jets, t jets]) 

p_neut :: DDecay 
p_neut = d ([1000022], [t adms, t jets, t jets, t jets])

idx_multijet :: CrossID ProcSmplIdx
idx_multijet = mkCrossIDIdx (mkDICross p_multijet )


map_multijet :: ProcSpecMap
map_multijet = 
    HM.fromList [(Nothing,"\ngenerate P P > go go QED=99\n")
                ,(Just (3,1000021,[]), "\ngenerate go > j j n1 QED=99\n")
                ,(Just (4,1000021,[]), "\ngenerate go > j j n1 QED=99\n")
                ,(Just (1,1000022,[3]), "\ngenerate n1 > u d d sxxp~ QED=99\n")
                ,(Just (1,1000022,[4]), "\ngenerate n1 > u d d sxxp~ QED=99\n")
                ] 




main :: IO () 
main = evchainGen map_multijet p_multijet 100

{-


-- | 
getLHEvents :: FilePath -> IO [LHEvent] 
getLHEvents fn = do 
  h <- openFile fn ReadMode 
  evts <- evtsHandle True h =$= CL.map fromJust $$ CL.consume 
  return evts 



-- | 
makeLHEProcessMap :: ProcessMap FilePath -> IO (ProcessMap [LHEvent])
makeLHEProcessMap = T.mapM getLHEvents 



-- | 
evchainGen :: ProcSpecMap -> DCross -> Int -> IO () 
evchainGen pmap cross n = do 
  let idxcross = (mkCrossIDIdx . mkDICross) cross 
  print idxcross
  rm <- createProcessX (generateX pmap) (generateD pmap) 
          lheCntX lheCntD idxcross n
  rm2 <- makeLHEProcessMap rm 
  let test = replicate n (accumTotalEvent <$> matchFullCross idx_multijet)
  let r = runState (runErrorT (sequence test)) rm2
  print (fst r)


-}
--  matchFullCross spec3_tbbar_idx 
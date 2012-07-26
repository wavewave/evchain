{-# LANGUAGE ScopedTypeVariables, RecordWildCards #-}

module Main where

import HEP.Parser.LHEParser.Type

import HEP.Automation.EventChain.Type.Skeleton
import HEP.Automation.EventChain.Type.Spec
import HEP.Automation.EventChain.SpecDSL


import qualified Data.IntMap as IM
import           System.Random 

-- type ProcessID = Int 

data DummyEvent = DummyEvent { inptls :: [PDGID]
                             , outptls :: [PDGID] } 
                deriving (Show,Eq)


data DummyProcess = DummyProcess { numEvent :: Int } 

type DummyMap = IM.IntMap DummyProcess 

generatePtl :: [PDGID] -> IO PDGID
generatePtl lst = do let n = length lst
                     r <- randomRIO (0,n-1)  
                     return (lst !! r)

generateEventG :: ([[PDGID]], [[PDGID]]) 
              -- ^ incoming particles,outgoing particles
              -> IO DummyEvent 
generateEventG (inlst,outlst) = do 
    ins <- mapM generatePtl inlst 
    outs <- mapM generatePtl outlst 
    return (DummyEvent ins outs) 
                       
generateEventSD :: SDecay -> IO DummyEvent 
generateEventSD (MkD dnode1 douts1) = generateEventG ([dnode1],daughters)
  where daughters = map getNodeD douts1 
 --        f (MkD {..}) = dnode
 --        f (MkT {..}) = tnode 
generateEventSD (MkT {..} ) = error "I cannot generate for terminal node" 


generateEventSX :: SCross -> IO DummyEvent 
generateEventSX (MkC xnode1 (xinc1,xinc2) xouts1) = do 
    generateEventG ([getNodeD xinc1, getNodeD xinc2],outs)
  where outs = map getNodeD xouts1
       

spec_test1 = d ([1,2,3],[1,2,d ([3,9,10],[2,2])])

spec_test2 = x (t proton,t proton,[spec_test1,1000021,1000021])

main = do 
  --- gen <- getStdGen 
  {- 
  (r1 :: Int ) <- randomRIO (0,10000) 
  (r2 :: Int ) <- randomRIO (0,r1) 
  (r3 :: Int ) <- randomRIO (0,r2) 
  
  let m = IM.fromList [ (1,r1) , (2,r2) , (3,r3) ] 
  print m
  -}
  {-
  r <- generateEvent ([[1,2,4],[7,8,9]], [[1000,1001,1002],[1003]])
  -}
  r <- generateEventSX spec_test2
  print r 


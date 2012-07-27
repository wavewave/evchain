{-# LANGUAGE ScopedTypeVariables, RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Automation.EventChain.Simulator
-- Copyright   : (c) 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- Simulating madgraph event generation. 
-- 
-----------------------------------------------------------------------------

module HEP.Automation.EventChain.Simulator where

-- other packages from others
import           Control.Applicative 
import           Control.Monad
import           Data.Foldable (foldrM)
import qualified Data.HashMap.Lazy as HM
import qualified Data.IntMap as IM
import           System.Random 
-- from other hep-platform packages 
import HEP.Parser.LHEParser.Type
-- from this package
import HEP.Automation.EventChain.Type.Skeleton
import HEP.Automation.EventChain.Type.Spec
import HEP.Automation.EventChain.SpecDSL
import HEP.Automation.EventChain.Process
-- 
import Debug.Trace

-- | 

data DummyEvent = DummyEvent { inptls :: [(ParticleID,PDGID)]
                             , outptls :: [(ParticleID,PDGID)] } 
                deriving (Show,Eq)

-- | 

data DummyProcess = DummyProcess { events :: [DummyEvent]
                                 } 
                  deriving (Show,Eq) 


-- | generate a single particle out of a list of possible PDGID choices 

generatePtl :: (ParticleID,[PDGID]) -> IO (ParticleID,PDGID)
generatePtl (pid,lst) = do let n = length lst
                           r <- randomRIO (0,n-1)  
                           return (pid,(lst !! r))

-- | generate event 

generateEventG :: ([(ParticleID,[PDGID])], [(ParticleID,[PDGID])]) 
              -- ^ incoming particles,outgoing particles
              -> IO DummyEvent 
generateEventG (inlst,outlst) = do 
    ins <- mapM generatePtl inlst 
    outs <- mapM generatePtl outlst 
    return (DummyEvent ins outs) 
                    
-- | generate event specified as SDecay 
   
generateEventSD :: SIDecay p -> IO DummyEvent 
generateEventSD (MkD dnode1 douts1) = generateEventG ([self],daughters)
  where extractpair = (,) <$> prinfoid_ptlid <*> prinfoid_pdgids
        expairfromnode MkD {..} = extractpair dnode 
        expairfromnode MkT {..} = tnode 
        self =  extractpair dnode1  
        daughters = map expairfromnode douts1 
generateEventSD (MkT {..} ) = error "I cannot generate for terminal node" 

-- | generate event specified as SCross

generateEventSX :: SICross p -> IO DummyEvent 
generateEventSX (MkC xnode1 (xinc1,xinc2) xouts1) = do 
    generateEventG ([i1,i2],outs)
  where extractpair = (,) <$> prinfoid_ptlid <*> prinfoid_pdgids
        expairfromnode MkD {..} = extractpair dnode 
        expairfromnode MkT {..} = tnode 
        i1 = expairfromnode xinc1 
        i2 = expairfromnode xinc2
        outs = map expairfromnode xouts1
-- | 

countProcess :: DummyProcess -> Counter
countProcess dproc = foldr countSingleEvent (Counter HM.empty HM.empty) (events dproc)  
 
-- | 

countSingleEvent :: DummyEvent -> Counter -> Counter 
countSingleEvent DummyEvent {..} (Counter incomingm outgoingm) = Counter rim rom 
  where f idx m = HM.insertWith (+) idx 1 m
        rim = foldr f incomingm inptls
        rom = foldr f outgoingm outptls 


-- | 

genX :: SICross p -> Int -> IO DummyProcess
genX c n = replicateM n (generateEventSX c) >>= return . DummyProcess 

-- | 

genD :: SIDecay p -> Int -> IO DummyProcess
genD d n = replicateM n (generateEventSD d) >>= return . DummyProcess
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

-- | Nothing for Cross, Just for Decay

type ProcessIndex = [(ParticleID,PDGID)] 

-- | 

type DummyMap = HM.HashMap ProcessIndex DummyProcess 

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
   
generateEventSD :: SIDecay -> IO DummyEvent 
generateEventSD (MkD dnode1 douts1) = generateEventG ([dnode1],daughters)
  where daughters = map getNodeD douts1 
generateEventSD (MkT {..} ) = error "I cannot generate for terminal node" 

-- | generate event specified as SCross

generateEventSX :: SICross -> IO DummyEvent 
generateEventSX (MkC xnode1 (xinc1,xinc2) xouts1) = do 
    generateEventG ([getNodeD xinc1, getNodeD xinc2],outs)
  where outs = map getNodeD xouts1
       

-- | create process for a decay 

createProcessD :: SIDecay -> ProcessIndex -> DummyMap -> [(PDGID,Int)] -> IO DummyMap 
createProcessD decay idxroot procm lst = foldrM (createProcessDwrk decay idxroot) procm lst

-- | 

createProcessDwrk :: SIDecay -> ProcessIndex -> (PDGID,Int) -> DummyMap -> IO DummyMap 
createProcessDwrk self@MkD {..} idxroot (pdgid',n) m
    | pdgid' `elem` snd dnode = do let nkey = (fst dnode,pdgid') : idxroot
                                   dproc <- return . DummyProcess =<< replicateM n go 
                                   let newmap = HM.insert nkey dproc m 
                                       cntr = countProcess dproc
                                   rmap <- foldrM (f nkey (outcounter cntr)) newmap douts
                                   return rmap 
    | otherwise = fail ("createProcessDwrk : cannot find pdgid = " ++ show pdgid'
                         ++ show self )
  where go = generateEventSD self {dnode = (fst dnode,[pdgid'])}
        f k cntrm dcy procm = createProcessD dcy k procm (mkOccNumDecay dcy cntrm) 

createProcessDwrk MkT {..}  _ _ m = return m 

-- | create process for a cross 

createProcessX :: SICross -> Int -> IO DummyMap 
createProcessX cross@MkC {..} n = do 
    dproc <- return . DummyProcess =<< replicateM n (generateEventSX cross)
    let cntr = countProcess dproc 
    let newmap = HM.insert [] dproc HM.empty 
        xinc1 = fst xincs 
        xinc2 = snd xincs 
        ilst = [xinc1, xinc2]
    rmap1 <- foldrM (f (incounter cntr)) newmap ilst 
    rmap2 <- foldrM (f (outcounter cntr)) rmap1 xouts 
    return rmap2 
  where f cntrm dcy procm = createProcessD dcy [] procm (mkOccNumDecay dcy cntrm)

{-
createProcessD decay root newmap 

   HM.lookup [(1,1)]
      cntr 


  createProcessD 
-}




-- | counting each particle id

data Counter = Counter { incounter :: HM.HashMap (ParticleID,PDGID) Int 
                       , outcounter :: HM.HashMap (ParticleID,PDGID) Int }
             deriving (Show,Eq)

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

mkOccNum :: ParticleID -> HM.HashMap (ParticleID,PDGID) Int -> [(PDGID,Int)] 
mkOccNum pid = map (\((_,pdg),n)->(pdg,n)) 
               . HM.toList 
               . HM.filterWithKey (\(i,_) _ -> i == pid)


mkOccNumDecay :: SIDecay -> HM.HashMap (ParticleID,PDGID) Int -> [(PDGID,Int)]
mkOccNumDecay MkT {..} _ = [] 
mkOccNumDecay MkD {..} cntr = mkOccNum (fst dnode) cntr 



 

{-
-- | count events in a decay 

countProcessD :: 

-}
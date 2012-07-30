{-# LANGUAGE ScopedTypeVariables, RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Automation.EventChain.Process
-- Copyright   : (c) 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- Recursively order event generation and counting. 
-- 
-----------------------------------------------------------------------------

module HEP.Automation.EventChain.Process where

-- other packages from others
import           Data.Foldable (foldrM)
import qualified Data.HashMap.Lazy as HM
-- from other hep-platform packages 
import           HEP.Parser.LHEParser.Type
-- from this package
import           HEP.Automation.EventChain.Type.Skeleton
import           HEP.Automation.EventChain.Type.Spec




-- | 

type ProcessMap a = HM.HashMap ProcessIndex a



-- | counting each particle id

data Counter = Counter { incounter :: HM.HashMap (ParticleID,PDGID) Int 
                       , outcounter :: HM.HashMap (ParticleID,PDGID) Int }
             deriving (Show,Eq)

  
-- |

mkOccNum :: ParticleID -> HM.HashMap (ParticleID,PDGID) Int -> [(PDGID,Int)] 
mkOccNum pid = map (\((_,pdg),n)->(pdg,n)) 
               . HM.toList 
               . HM.filterWithKey (\(i,_) _ -> i == pid)


mkOccNumDecay :: DecayID p -> HM.HashMap (ParticleID,PDGID) Int -> [(PDGID,Int)]
mkOccNumDecay MkT {..} _ = [] 
mkOccNumDecay MkD {..} cntr = mkOccNum (ptl_ptlid dnode) cntr 


-- | create process for a decay 

createProcessD :: (Monad m) => 
                  (DecayID p -> Int -> m a) 
               -> (a -> m Counter)
               -> DecayID p 
               -> ProcessIndex 
               -> ProcessMap a 
               -> [(PDGID,Int)] 
               -> m (ProcessMap a)
createProcessD gen cnt decay idxroot procm lst = 
    foldrM (createProcessDwrk gen cnt decay idxroot) procm lst

-- | 

createProcessDwrk :: (Monad m) =>
                     (DecayID p -> Int -> m a) -- ^ generator function for decay 
                  -> (a -> m Counter)          -- ^ counter function for a 
                  -> DecayID p
                  -> ProcessIndex 
                  -> (PDGID,Int) 
                  -> ProcessMap a 
                  -> m (ProcessMap a)
createProcessDwrk _gen _cnt MkT {..}  _ _ m = return m 
createProcessDwrk gen cnt self@MkD {..} idxroot (pdgid',n) m 
    = case lookupid pdgid' (ptl_procs dnode) of 
        Nothing -> fail ("createProcessDwrk : cannot find pdgid = " ++ show pdgid') --  ++ show self )
        Just prpdg -> do 
          let nkey = (ptl_ptlid dnode,pdgid') : idxroot
          dproc <- gen self {dnode=dnode { ptl_procs = [prpdg] }} n 
          let newmap = HM.insert nkey dproc m 
          cntr <- cnt dproc
          rmap <- foldrM (f nkey (outcounter cntr)) newmap douts
          return rmap 
  where f k cntrm dcy procm = createProcessD gen cnt dcy k procm (mkOccNumDecay dcy cntrm) 



--    | pdgid' `elem` (map proc_pdgid (ptl_procs dnode)) = 


-- | create process for a cross 

createProcessX :: (Monad m) => 
                  (CrossID p -> Int -> m a) -- ^ generator function for cross
               -> (DecayID p -> Int -> m a) -- ^ generator function for decay 
               -> (a -> m Counter)          -- ^ counter function 
               -> CrossID p
               -> Int 
               -> m (ProcessMap a)
createProcessX genX genD cnt cross@MkC {..} n = do 
    dproc <- genX cross n
    cntr <- cnt dproc 
    let newmap = HM.insert [] dproc HM.empty 
        xinc1 = fst xincs 
        xinc2 = snd xincs 
        ilst = [xinc1, xinc2]
    rmap1 <- foldrM (f (incounter cntr)) newmap ilst 
    rmap2 <- foldrM (f (outcounter cntr)) rmap1 xouts 
    return rmap2 
  where f cntrm dcy procm = createProcessD genD cnt dcy [] procm (mkOccNumDecay dcy cntrm)


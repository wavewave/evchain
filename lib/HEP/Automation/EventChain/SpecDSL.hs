{-# LANGUAGE GADTs, EmptyDataDecls, FlexibleInstances, StandaloneDeriving,
             TypeSynonymInstances, RankNTypes, TupleSections, RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Automation.EventChain.SpecDSL
-- Copyright   : (c) 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- Types for Spec DSL 
--
-----------------------------------------------------------------------------

module HEP.Automation.EventChain.SpecDSL where

import HEP.Parser.LHEParser.Type (PDGID)
import HEP.Automation.EventChain.Type.Skeleton
import HEP.Automation.EventChain.Type.Spec

-----------------------------------------------------------------------------
-- Collections
-----------------------------------------------------------------------------

-- | particle-antiparticle pair

ppair :: PDGID -> [PDGID]
ppair n = [ n, -n ]

-- | a collection of jets 

jet :: [PDGID]
jet = [ 1,-1,2,-2,3,-3,4,-4,21 ] 

-- | proton 

proton :: [PDGID] 
proton = jet 

-- | antiproton 

antiproton :: [PDGID] 
antiproton = jet 

-- | a collection of charged leptons 

lepton :: [PDGID] 
lepton = [ 11,-11, 13, -13, 15, -15 ]

-- | a collection of neutrinos

neutrino :: [PDGID]
neutrino = [ 12,-12, 14,-14,16,-16 ]



-----------------------------------------------------------------------------
-- Simple Spec
-----------------------------------------------------------------------------

-- | create simple spec topology (cross)

x :: (DDecay,DDecay,[DDecay]) -> DCross 
x (a,b,ys) = MkC () (a,b) ys 

-- | create simple spec topology (decay)

d :: ([PDGID],[DDecay]) -> DDecay
d (a,ys) = MkD a ys

-- | creates simple spec (as a general function)

t :: tnode -> Decay dnode tnode
t a = MkT a 



-----------------------------------------------------------------------------
-- Spec with Process Info 
-----------------------------------------------------------------------------

-- | create spec process with process 
xp :: p -> (SPDecay p,SPDecay p,[SPDecay p]) -> SPCross p 
xp p (a,b,ys) = MkC p (a,b) ys 

-- | create simple spec topology (decay)

dp :: [p] -> ([PDGID],[SPDecay p]) -> SPDecay p
dp ps (ids,ys) = MkD (zip ps ids) ys

-----------------------------------------------------------------------------
-- Generate Spec by common process info 
-----------------------------------------------------------------------------

-- | 

xc :: (p,SDecay p,SDecay p,[SDecay p]) -> SCross p 
xc (p,i1,i2,outs) = MkC p (i1,i2) outs 

-- | 

dc :: (p,[PDGID],[SDecay p]) -> SDecay p 
dc (p,ids,ys) = MkD (p,ids) ys




-----------------------------------------------------------------------------
-- transform simple spec into spec with ptlid 
-----------------------------------------------------------------------------

mkDIDecay :: ParticleID -> DDecay -> DIDecay
mkDIDecay pid MkT {..} = MkT (pid,tnode) 
mkDIDecay pid MkD {..} = MkD (pid,dnode) (zipWith mkDIDecay [1..] douts)

-- | 

mkDICross :: DCross -> DICross
mkDICross MkC {..} = MkC () (inc1,inc2) outs 
  where inc1 = mkDIDecay 1 (fst xincs) 
        inc2 = mkDIDecay 2 (snd xincs)
        outs = zipWith mkDIDecay [3..] xouts

-- | 

mkSIDecay :: ParticleID -> SDecay p -> SIDecay p
mkSIDecay pid MkT {..} = MkT (pid,tnode) 
mkSIDecay pid MkD {..} = MkD (PrInfoID (fst dnode) pid (snd dnode))
                           (zipWith mkSIDecay [1..] douts)

-- | 

mkSICross :: SCross p -> SICross p
mkSICross MkC {..} = MkC xnode (inc1,inc2) outs 
  where inc1 = mkSIDecay 1 (fst xincs) 
        inc2 = mkSIDecay 2 (snd xincs)
        outs = zipWith mkSIDecay [1..] xouts

-- | 

mkCrossIDIdx :: DICross -> CrossID ProcSmplIdx 
mkCrossIDIdx MkC {..} = MkC [] (i1,i2) outs
  where xinc1 = fst xincs
        xinc2 = snd xincs  
        i1 = mkDecayIDIdx [] xinc1 
        i2 = mkDecayIDIdx [] xinc2 
        outs = map (mkDecayIDIdx []) xouts 

-- | 

mkDecayIDIdx :: ProcSmplIdx -> DIDecay -> DecayID ProcSmplIdx
mkDecayIDIdx idxroot MkT {..} = MkT tnode 
mkDecayIDIdx idxroot MkD {..} = MkD ndnode (map (mkDecayIDIdx nidx) douts) 
  where pid = fst dnode 
        pdgids = snd dnode 
        nidx = pid:idxroot 
        ndnode = PtlProcPDG pid (map mkidx pdgids) 
        mkidx :: PDGID -> ProcPDG ProcSmplIdx 
        mkidx i = ProcPDG nidx i



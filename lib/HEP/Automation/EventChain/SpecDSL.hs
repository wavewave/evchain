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

x :: (SDecay,SDecay,[SDecay]) -> SCross 
x (a,b,ys) = MkC () (a,b) ys 

-- | create simple spec topology (decay)

d :: ([PDGID],[SDecay]) -> SDecay
d (a,ys) = MkD a ys

-- | creates simple spec (as a general function)

t :: tnode -> Decay dnode tnode
t a = MkT a 

-----------------------------------------------------------------------------
-- transform simple spec into spec with ptlid 
-----------------------------------------------------------------------------

mkSIDecay :: ParticleID -> SDecay -> SIDecay
mkSIDecay pid MkT {..} = MkT (pid,tnode) 
mkSIDecay pid MkD {..} = MkD (pid,dnode) (zipWith mkSIDecay [1..] douts)

-- | 

mkSICross :: SCross -> SICross
mkSICross MkC {..} = MkC () (inc1,inc2) outs 
  where inc1 = mkSIDecay 1 (fst xincs) 
        inc2 = mkSIDecay 2 (snd xincs)
        outs = zipWith mkSIDecay [1..] xouts






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
dpcom :: p -> ([PDGID], [SPDecay p]) -> SPDecay p 
dpcom p (ids,ys) = MkD (map (p,) ids) ys






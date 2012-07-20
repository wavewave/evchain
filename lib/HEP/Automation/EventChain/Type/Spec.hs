{-# LANGUAGE GADTs, EmptyDataDecls, FlexibleInstances, StandaloneDeriving,
             TypeSynonymInstances, RankNTypes #-}

-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Automation.EventChain.Type.Spec
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

module HEP.Automation.EventChain.Type.Spec where

import HEP.Parser.LHEParser.Type (PDGID)
import HEP.Automation.EventChain.Type.Skeleton

type ProcessID = Int

type SCross = Cross () (CVec PDGID) (CVec PDGID)

type SDecay = Decay (CVec PDGID) (CVec PDGID)

type SPCross = Cross ProcessID (CVec2 PDGID ProcessID) (CVec PDGID) 




{-
-- | 

type PDGID = Int 


-- | 

data Cardinality = Single | Dual | Multiple 


-- | 

data PKind (c :: Cardinality) where 
  KPDGID        :: PDGID -> PKind Single
  MultiJet      :: PKind Multiple
  MultiLepton   :: PKind Multiple 
  MultiNeutrino :: PKind Multiple  
  PtlPtlbar     :: PDGID -> PKind Dual 

-- | 

data Collection (c :: Cardinality) a where
  Singlet :: a -> Collection Single a 
  Duet :: a -> a -> Collection Dual a 
  List :: [a] -> Collection Multiple a  

-}

{-

type SDecayTop = GDecayTop DNode TNode PKindWrapper ProcessID

type SCross = forall a. GCross XNode DNode TNode PKindWrapper ProcessID 

instance Num (SDecayTop) where
  _ + _ = error " no + defined for SDecayTop"
  -- _ - _ = error " no - defined for SDecayTop"
  negate (GTerminal (TNode (KPDGID n))) = GTerminal (TNode (KPDGID (-n)))
  negate _ = error " no negate defined for SDecayTop in general"
  _ * _ = error " no * defined for SDecayTop"
  abs _ = error " no abs defined for SDecayTop"
  signum _ = error " no signum defined for SDecayTop"  
  fromInteger n = GTerminal (TNode (PKindWrapper (KPDGID (fromInteger n))))

-- | 

jet :: SDecayTop 
jet = GTerminal (TNode (PKindWrapper MultiJet))

-- |
 
lepton :: SDecayTop
lepton = GTerminal (TNode (PKindWrapper MultiLepton))

-- |

neutrino :: SDecayTop
neutrino = GTerminal (TNode (PKindWrapper MultiNeutrino))

-- |

ppair :: PDGID -> SDecayTop 
ppair pdg_id = GTerminal (TNode (PKindWrapper (PtlPtlbar pdg_id)))

-- | 

x :: ProcessID -> (SDecayTop,SDecayTop,[SDecayTop]) -> SCross
x procid (a,b,ys) =  GCross (XNode procid) [a,b] ys 

-- |

d :: ProcessID -> (PKind,[SDecayTop]) -> SDecayTop 
d procid (a,ys) = GDecay (DNode a procid,ys)

-- | 

makeDecayID :: ParticleID -> SDecayTop -> DecayID
makeDecayID idnum (GTerminal (TNode n)) = GTerminal (TNode (idnum,n))
makeDecayID idnum (GDecay (DNode n procid,ys)) = GDecay (DNode (idnum,n) procid,dtable)
  where dtable = zipWith makeDecayID [1..] ys

-- | 

makeCrossID :: SCross -> CrossID
makeCrossID (GCross procid inc out) = GCross procid incnew outnew 
  where incnew = zipWith makeDecayID [1..] inc
        outnew = zipWith makeDecayID [length inc+1..] out


-}
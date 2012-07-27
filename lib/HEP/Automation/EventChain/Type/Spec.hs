{-# LANGUAGE GADTs, EmptyDataDecls, FlexibleInstances, StandaloneDeriving,
             TypeSynonymInstances, RankNTypes, TypeOperators #-}

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

-- from other hep-platform packages 
import HEP.Parser.LHEParser.Type (PDGID)
-- from this package
import HEP.Automation.EventChain.Type.Skeleton

-- | process id for identifying LHE files

type ProcessID = Int

-- | status : -1 : incoming, 1 : outgoing, 2 : intermediate

type Status = Int

-- | 

statusIn :: Status
statusIn = -1 

-- | 

statusOut :: Status
statusOut = 1

-- | 

statusIntermediate :: Status
statusIntermediate = 2


-- | tag for incoming / outgoing direction

data InOutDir = In | Out 
              deriving Show 


-- | particle id in a given LHE file. (different from PDGID)

type ParticleID = Int

-- |

type SCross = Cross () [PDGID] [PDGID]

-- |

type SDecay = Decay [PDGID] [PDGID]


-- | 

type SICross = Cross () (ParticleID,[PDGID]) (ParticleID,[PDGID])

-- | 

type SIDecay = Decay (ParticleID,[PDGID]) (ParticleID,[PDGID]) 

-- | 

-- | 

type SPCross p = Cross p [(p,PDGID)] [PDGID]

-- | 

type SPDecay p = Decay [(p,PDGID)] [PDGID] 


-- | process and selection function pair  

data ProcPDG = ProcPDG { proc_procid :: ProcessID
                       , proc_pdgid :: PDGID } 
             deriving (Show,Eq)

-- | 

data PtlProcPDG = PtlProcPDG { ptl_ptlid :: ParticleID 
                             , ptl_procs :: [ProcPDG] }
                  deriving (Show,Eq)

-- | type for cross process with only ids 

type CrossID = Cross ProcessID PtlProcPDG (ParticleID,[PDGID])

-- | type for decay process with only ids

type DecayID = Decay PtlProcPDG (ParticleID,[PDGID])


-- |
 
instance Num (Decay a [PDGID] ) where
  _ + _ = error " no + defined for SDecay"
  negate (MkT [n]) = MkT [-n]
  negate _ = error " no negate defined for SDecay in general" 
  _ * _ = error " no * defined for SDecay"
  abs _ = error " no abs defined for SDecay"
  signum _ = error " no signum defined for SDecay"  
  fromInteger n = MkT [fromInteger n]




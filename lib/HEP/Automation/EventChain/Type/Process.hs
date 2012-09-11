{-# LANGUAGE GADTs, EmptyDataDecls, FlexibleInstances, StandaloneDeriving,
             TypeSynonymInstances, RankNTypes, TupleSections, RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Automation.EventChain.Type.Process
-- Copyright   : (c) 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- Types for Process Spec and Generator 
--
-----------------------------------------------------------------------------

module HEP.Automation.EventChain.Type.Process where

-- import           Data.Hashable 
import qualified Data.HashMap.Lazy as HM

import           HEP.Parser.LHEParser.Type 

import           HEP.Automation.EventChain.Type.Spec 

-----------------------------------------------------------------------------
-- with process info 
-----------------------------------------------------------------------------

-- | 
type ProcessIndex = [(ParticleID,PDGID)] 

-- | 
type ProcSmplIdx = [ParticleID]

-- | 
type ProcSpecIdx = Maybe (ParticleID,PDGID,ProcSmplIdx) 

-- | 
type ProcSpecMap = HM.HashMap ProcSpecIdx ProcessInfo 

-- | 
type ProcessMap a = HM.HashMap ProcessIndex a

-- type GeneratorM m = StateT ProcessTable m 

-- | process id for identifying LHE files

type ProcessID = Int

-- | Process Info  

type ProcessInfo = String 

-- | 



-- | 

mkPMIdx :: ProcSmplIdx -> PDGID -> ProcSpecIdx 
mkPMIdx [] _ = error "cannot have this case."
mkPMIdx (i:is) pdgid' = Just (i,pdgid',is)

-- | 

-- mkDecayIDProc :: Pro





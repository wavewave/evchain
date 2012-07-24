{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Automation.EventChain.Type.Match
-- Copyright   : (c) 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- Utility functions for matching a lhe format data with a given spec 
-- 
-----------------------------------------------------------------------------

module HEP.Automation.EventChain.Type.Match where

-- from other packages from others 
import           Control.Monad.Error
import           Control.Monad.State
import qualified Data.Map as M
-- from other hep-platform package
import           HEP.Parser.LHEParser.Type
-- from this package
import           HEP.Automation.EventChain.Type.Spec

-- | match function type to find a particle with a given pdgid and 
--   status criterion

data SelectFunc = SelectFunc { selectFunc :: (PDGID,Status) -> Bool 
                             , description :: String }

-- | data type for a single LHE event matched with a specified process node 

data MatchedLHEvent = MLHEvent { mlhev_procid :: ProcessID
                               , mlhev_orig :: LHEvent 
                               , mlhev_einfo :: EventInfo
                               , mlhev_incoming :: [(ParticleID,PtlInfo)]
                               , mlhev_outgoing :: [(ParticleID,PtlInfo)]
                               , mlhev_intermediate :: [PtlInfo] } 

-- | coordinate for a particle in a given collection of processes 

type ParticleCoord = (ProcessID,ParticleID)

-- | coord storage for particles  

type ParticleCoordMap = M.Map ParticleCoord PtlID 

-- | (ProcessID,SelectFunc)

data ProcSel = ProcSel { procsel_procid :: ProcessID
                       , procsel_sel :: SelectFunc } 

-- | (ParticleID,[(ProcessID,SelectFunc)])

data PtlProcSel = PtlProcSel { ptlsel_ptlid :: ParticleID
                             , ptlsel_procsels :: ProcSel } 


type MatchM = ErrorT String (State [PtlInfo]) 
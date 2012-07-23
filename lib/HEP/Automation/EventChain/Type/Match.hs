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

-- from other hep-platform package
import HEP.Parser.LHEParser.Type
-- from this package
import HEP.Automation.EventChain.Type.Spec

-- | data type for a single LHE event matched with a specified process node 

data MatchedLHEvent = MLHEvent { mlhev_procid :: ProcessID
                               , mlhev_orig :: LHEvent 
                               , mlhev_einfo :: EventInfo
                               , mlhev_incoming :: [(ParticleID,PtlInfo)]
                               , mlhev_outgoing :: [(ParticleID,PtlInfo)]
                               , mlhev_intermediate :: [PtlInfo] } 

{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Automation.EventChain.Match
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

module HEP.Automation.EventChain.Match where

-- from other packages 
import           Data.Foldable (foldrM)
-- other hep-platform package 
import           HEP.Parser.LHEParser.Type
-- from this package
import           HEP.Automation.EventChain.Type.Spec

-- | match function type to find a particle with a given pdgid and 
--   status criterion

data SelectFunc = SelectFunc { selectFunc :: (PDGID,Status) -> Bool 
                             , description :: String }


-- | match a particle using a selection criterion

findPtlWSelect :: SelectFunc                 -- ^ selector function 
               -> [PtlInfo]                  -- ^ initial list 
               -> (Maybe PtlInfo,[PtlInfo])  -- ^ (matched, unmatched) 
findPtlWSelect sel pinfos = foldr f (Nothing,[]) pinfos 
  where matchf = selectFunc sel
        f pinfo (Just matched,unmatched) = (Just matched, pinfo:unmatched)
        f pinfo (Nothing,unmatched) = if matchf (idup pinfo, istup pinfo) 
                                        then (Just pinfo,unmatched)
                                        else (Nothing,pinfo:unmatched)


-- | sequentially finding all particles by a list of selection criterions

matchAllPtlWSelect :: [(ParticleID,SelectFunc)] 
                   -> [PtlInfo]
                   -> Either String ([(ParticleID,PtlInfo)],[PtlInfo]) 
                          -- ^ error monad, returning matched with ParticleId 
                          --   and unmatched (no id) 
matchAllPtlWSelect ls pinfos = foldrM f ([],pinfos) ls
  where f (pid,sel) (done,remaining) = 
          case findPtlWSelect sel remaining of
            (Nothing,_)             -> Left ((description sel) ++ " cannot be found.")
            (Just pinfo,remaining') -> return ((pid,pinfo):done,remaining')


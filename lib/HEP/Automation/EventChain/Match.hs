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
import           Control.Applicative 
import           Control.Monad.Error
import           Control.Monad.State
-- import           Data.Foldable (foldrM)
-- other hep-platform package 
import           HEP.Parser.LHEParser.Type
-- from this package
import           HEP.Automation.EventChain.Type.Match
import           HEP.Automation.EventChain.Type.Skeleton
import           HEP.Automation.EventChain.Type.Spec


{-            -> [PtlInfo]                  -- ^ initial list 
               -> (Maybe PtlInfo,[PtlInfo])  -- ^ (matched, unmatched)  -}

-- | match a particle using a selection criterion

findPtlBy :: SelectFunc                 -- ^ selector function 
             -> MatchM PtlInfo 
findPtlBy sel = do 
    (pinfos1,pinfos2) <- return . break (selectFunc sel . ((,) <$> idup <*> istup)) =<< lift get 
    if null pinfos2 
      then fail  ("not found when " ++ description sel)
      else (lift . put) (pinfos1 ++ tail pinfos2) >> return (head pinfos2)


-- | sequentially finding all particles by a list of selection criterions

findPtls:: [(ParticleID,SelectFunc)] -> MatchM [(ParticleID,PtlInfo)]
findPtls lst = return . zip (map fst lst) =<<  mapM (findPtlBy.snd) lst
        


-- | match incoming and outgoing particles with a given id set from a given 
--   event file

matchInOut :: ProcessID 
           -> ([(ParticleID,SelectFunc)],[(ParticleID,SelectFunc)]) 
           -> LHEvent 
           -> Either String MatchedLHEvent
matchInOut procid (is,os) ev@(LHEvent einfo pinfos) = evalState match pinfos 
  where match :: State [PtlInfo] (Either String MatchedLHEvent)
        match = runErrorT $ do 
                  iinfos <- findPtls is
                  oinfos <- findPtls os 
                  lift get >>= return . MLHEvent procid ev einfo iinfos oinfos





{-


-- | 

matchPtl4Cross :: CrossID
               -> LHEvent
               -> Either String MatchedLHEvent 
matchPtl4Cross (MkC procid (inc1,inc2) out) lhe = matchInOut procid (incids,outids) lhe
  where pairPtlSel = (,) <$> ptlsel_ptlid <*> (procsel_sel . ptlsel_procsels)
        incids = map (pairPtlSel . getPtlProcSel In)  [inc1,inc2]
        outids = map (pairPtlSel . getPtlProcSel Out) out 

-- | make a select function for one PDGID

mk1Sel :: InOutDir -> PDGID -> SelectFunc
mk1Sel dir pdgid = let f (pdgid',st) = isMatchedStatus st && pdgid' == pdgid
                    in SelectFunc f ("(pdgid = " ++ show pdgid ++ "," ++ show dir ++ ")")
  where isMatchedStatus st = case dir of 
                               In -> st == statusIn 
                               Out -> st == statusOut

-- | 

getPtlProcSel :: InOutDir -> DecayID -> PtlProcSel
getPtlProcSel dir x = let PtlProcPDG pid procs = dnode x  
                      in PtlProcSel pid (map (ProcSel<$>proc_procid<*>(mk1Sel dir).proc_pdgid) procs)


-}
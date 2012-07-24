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

findPtl :: SelectFunc                 -- ^ selector function 
           -> MatchM PtlInfo 
findPtl sel = do 
    (pinfos1,pinfos2) <- return . break (selectFunc sel . ((,) <$> idup <*> istup)) =<< lift get 
    if null pinfos2 
      then fail  ("not found when " ++ description sel)
      else (lift . put) (pinfos1 ++ tail pinfos2) >> return (head pinfos2)

-- | sequentially finding all particles by a list of selection criterions

findPtls:: [(ParticleID,SelectFunc)] -> MatchM [(ParticleID,PtlInfo)]
findPtls lst = return . zip (map fst lst) =<<  mapM (findPtl.snd) lst
        
{-

           -> [PtlInfo]
                   -> Either String ([(ParticleID,PtlInfo)],[PtlInfo]) 
                          -- ^ error monad, returning matched with ParticleId 
                          --   and unmatched (no id) 
matchAllPtlWSelect ls pinfos = foldrM f ([],pinfos) ls
  where f (pid,sel) (done,remaining) = 
          case findPtlWSelect sel remaining of
            (Nothing,_)             -> Left ((description sel) ++ " cannot be found.")
            (Just pinfo,remaining') -> return ((pid,pinfo):done,remaining')

-}

{-
-- | match incoming and outgoing particles with a given id set from a given 
--   event file

matchInOut :: ProcessID 
           -> ([PtlProcSel],[PtlProcSel]) 
           -> LHEvent 
           -> Either String MatchedLHEvent
matchInOut procid (incids,outids) ev@(LHEvent einfo pinfos) = do 
    (matched_inc,remaining)  <- matchAllPtlWSelect incids pinfos
    (matched_out,remaining') <- matchAllPtlWSelect outids remaining
    return (MLHEvent procid ev einfo matched_inc matched_out remaining')


-- | 

matchPtl4Cross :: CrossID
               -> LHEvent
               -> Either String MatchedLHEvent 
matchPtl4Cross (MkC procid inc out) lhe = matchInOut procid (incids,outids) lhe
  where incids = map (getSelPair In)  inc
        outids = map (getSelPair Out) out 

-- | make a select function for one PDGID

mk1Sel :: InOutDir -> PDGID -> SelectFunc
mk1Sel dir pdgid = let f (pdgid',st) = isMatchedStatus st && pdgid' == pdgid
                    in SelectFunc f ("(pdgid = " ++ show pdgid ++ "," ++ show dir ++ ")")
  where isMatchedStatus st = case dir of 
                               In -> st == statusIn 
                               Out -> st == statusOut

-- | 

getSelPair :: InOutDir -> DecayID -> (ParticleID,[(ProcessID,SelectFunc)])
getSelPair dir x = let (pid,procs) = ( (,) <$> ptl_ptlid <*> ptl_procs ) (dnode x)  
                   in (pid, map ( (,) <$> proc_procid <*> (mk1Sel dir).proc_pdgid ) procs )

-}

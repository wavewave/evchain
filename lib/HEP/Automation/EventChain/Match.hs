{-# LANGUAGE ScopedTypeVariables, RecordWildCards, TupleSections #-}

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
import qualified Data.IntMap as IM
-- other hep-platform package 
import           HEP.Parser.LHEParser.Type
-- from this package
import           HEP.Automation.EventChain.Type.Match
import           HEP.Automation.EventChain.Type.Skeleton
import           HEP.Automation.EventChain.Type.Spec

-- | 

match1 :: InOutDir -> PDGID -> MatchM PtlInfo
match1 dir pdgid = do 
    (pinfos1,pinfos2) <- return . break (f . ((,) <$> idup <*> istup)) =<< lift get 
    if null pinfos2 
      then fail  ("not found : " ++ show pdgid)
      else (lift . put) (pinfos1 ++ tail pinfos2) >> return (head pinfos2)
  where f (pdgid',st) = isMatchedStatus st && pdgid' == pdgid 
        isMatchedStatus st = case dir of 
                               In -> st == statusIn 
                               Out -> st == statusOut

-- |

actT :: InOutDir -> (ParticleID,[PDGID]) -> MatchM (ParticleID,PtlInfo) 
actT dir (pid,ids) = (pid,) <$> msum (map (match1 dir) ids)

-- |

actD :: InOutDir -> PtlProcPDG -> MatchM (ParticleID,(ProcessID,PtlInfo))
actD dir PtlProcPDG {..} = (ptl_ptlid,) <$> msum (map m ptl_procs) 
  where m proc = (proc_procid proc,) <$> match1 dir (proc_pdgid proc) 

-- | 

getX :: (DecayID,DecayID,[DecayID]) -> MatchM MatchInOut
getX (in1,in2,outs) = do in1' <- getD In in1 
                         in2' <- getD In in2 
                         outs' <- mapM (getD Out) outs 
                         rem <- lift get
                         return (MIO [in1',in2'] outs' rem)

-- |

getD :: InOutDir -> DecayID -> MatchM (Either (ParticleID,PtlInfo) (ParticleID,(ProcessID,PtlInfo)))
getD dir MkT {..} = Left <$> actT dir tnode 
getD dir MkD {..} = Right <$> actD dir dnode


-- | 

getMatchedLHEvent :: ProcessID -> LHEvent -> MatchInOut -> MatchedLHEventProcess
getMatchedLHEvent procid ev@(LHEvent einfo pinfos) (MIO ins outs rs) = 
    MLHEvent procid ev einfo (map (either tnodef dnodef) ins) (map (either tnodef dnodef) outs) rs 
  where tnodef :: (ParticleID,PtlInfo) -> (Either ParticleID (ParticleID,ProcessID), PtlInfo)
        tnodef (pid,pinfo) = (Left pid, pinfo)
        
        dnodef :: (ParticleID,(ProcessID,PtlInfo)) -> (Either ParticleID (ParticleID,ProcessID), PtlInfo)
        dnodef (pid,(procid,pinfo)) = (Right (pid,procid), pinfo)

-- | 

matchX :: CrossID -> LHEvent -> Either String MatchedLHEventProcess 
matchX (MkC {..}) ev@(LHEvent einfo pinfos) = evalState (runErrorT m) pinfos
  where m = do let (in1,in2) = xincs
               mio <- getX (in1,in2,xouts)  
               return (getMatchedLHEvent xnode ev mio)

{-
-- | 



cascadeMatchX :: CrossID -> M.Map 

-- | 

matchD :: DecayID -> LHEvent -> Either String MatchedLHEventProcess 
matchD (MkD {..}) ev = evalState (runErrorT m) pinfos 
  where m = do i' <- getD In dnode  
               douts <- 
              
      
-}

{-
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
        
-- |

matchInOutG :: ([MatchM a],[MatchM a]) -> MatchM (([a],[a]),[PtlInfo])
matchInOutG (is,os) = do is' <- sequence is
                         os' <- sequence os 
                         lift get >>= return . (,) (is',os')

-- | match incoming and outgoing particles with given selection functions

matchInOut :: ([SelectFunc],[SelectFunc]) 
           -> MatchM (([PtlInfo],[PtlInfo]),[PtlInfo])  -- ^ ((matchedIn,matchedOut),unmatched)
matchInOut (is,os) = matchInOutG (map findPtlBy is, map findPtlBy os)

-- | 

matchCross :: CrossID -> LHEvent -> Either String (MatchedLHEvent (ParticleID,ProcessID) ParticleID)
matchCross (MkC procid (inc1,inc2) outs) ev@(LHEvent einfo pinfos) = evalState (runErrorT mact) pinfos
  where selm d = let procsels = (ptlsel_procsels . getProcSel In) d 
                     findfunc x = (procsel_procid x,) <$> (findPtlBy . procsel_sel) x 
                 in msum (map findfunc procsels)
        findpair d@MkD {..} = do (proc,info) <- selm d     
                                 return (Left (ptl_ptlid dnode,proc),info)
        findpair x@MkT {..} = do (proc,info) <- selm d 
                           (,) (Right (ptl_ptlid


        mact = do ((is,os),rs) <- matchInOutG ( [ findpair inc1, findpair inc2]
                                              , map findpair outs ) 
                  return (MLHEvent procid ev einfo is os rs)


-- msum . map (findPtlBy . procsel_sel) . ptlsel_procsels . getPtlProcSel In


-- | make a select function for one PDGID

mk1Sel :: InOutDir -> PDGID -> SelectFunc
mk1Sel dir pdgid = let f (pdgid',st) = isMatchedStatus st && pdgid' == pdgid
                    in SelectFunc f ("(pdgid = " ++ show pdgid ++ "," ++ show dir ++ ")")
  where isMatchedStatus st = case dir of 
                               In -> st == statusIn 
                               Out -> st == statusOut
-- | 

mkNSel :: InOutDir -> [PDGID] -> SelectFunc 


-- | 

getPtlProcSel :: InOutDir -> DecayID -> Either PtlProcSel (ParticleID,SelectionFunc)
getPtlProcSel dir MkD{..} = let PtlProcPDG pid procs = dnode   
                            in Left (PtlProcSel pid (map (ProcSel<$>proc_procid<*>(mk1Sel dir).proc_pdgid) procs))
getPtlProcSel dir MkT{..} = 





{-


-- | 

matchPtl4Cross :: CrossID
               -> LHEvent
               -> Either String MatchedLHEvent 
matchPtl4Cross (MkC procid (inc1,inc2) out) lhe = matchInOut procid (incids,outids) lhe
  where pairPtlSel = (,) <$> ptlsel_ptlid <*> (procsel_sel . ptlsel_procsels)
        incids = map (pairPtlSel . getPtlProcSel In)  [inc1,inc2]
        outids = map (pairPtlSel . getPtlProcSel Out) out 


-}
-}
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
-- import qualified Data.IntMap as IM
-- other hep-platform package 
import           HEP.Parser.LHEParser.Type
-- from this package
import           HEP.Automation.EventChain.Type.Match
import           HEP.Automation.EventChain.Type.Skeleton
import           HEP.Automation.EventChain.Type.Spec

-- | msum for supporting custom error message 

msum' :: (Monad m) => String -> [MatchM m a] -> MatchM m a 
msum' msg = foldr mplus (ErrorT (return (Left msg)))

-- | match a single particle with PDGID with incoming/outgoing status 

match1 :: (Functor m, Monad m) => InOutDir -> PDGID -> MatchM m PtlInfo
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

matchOr :: (Functor m, Monad m, Show a) => String -> (a -> MatchM m b) -> [a] -> MatchM m b 
matchOr msg m xs = msum' (msg ++ ": no match in matchOr with " ++ show xs) (map m xs)

-- |

actT :: (Functor m, Monad m) => InOutDir -> (ParticleID,[PDGID]) -> MatchM m (ParticleID,PtlInfo) 
actT dir (pid,ids) = (pid,) <$> matchOr "actT" (match1 dir) ids

--  msum' "no match in actT" (map (match1 dir) ids)

-- |

actD :: (Functor m, Monad m, Show p) => InOutDir -> PtlProcPDG p -> MatchM m (ParticleID,(p,PtlInfo))
actD dir PtlProcPDG {..} = (ptl_ptlid,) <$> matchOr "actD" m ptl_procs
  where m proc = (proc_procid proc,) <$> match1 dir (proc_pdgid proc) 


-- msum' "no match in actD" (map m ptl_procs)

-- |

checkD :: (Functor m, Monad m, Show p) => InOutDir -> DecayID p 
     -> MatchM m (Either (ParticleID,PtlInfo) (ParticleID,(p,PtlInfo)))
checkD dir MkT {..} = Left <$> actT dir tnode 
checkD dir MkD {..} = Right <$> actD dir dnode

-- | 

getX :: (Functor m, Monad m, Show p) => 
        (DecayID p, DecayID p, [DecayID p]) 
     -> MatchM m (MatchInOut p)
getX (in1,in2,outs) = do in1' <- checkD In in1 
                         in2' <- checkD In in2 
                         outs' <- mapM (checkD Out) outs 
                         rs <- lift get
                         return (MIO [in1',in2'] outs' rs)

-- | 

getMatchedLHEvent :: (Show p) => 
                     p -> LHEvent -> MatchInOut p -> MatchedLHEventProcess p
getMatchedLHEvent p ev@(LHEvent einfo _pinfos) (MIO ins outs rs) = 
    MLHEvent p ev einfo (map (either tnodef dnodef) ins) 
        (map (either tnodef dnodef) outs) rs 
  where tnodef :: (ParticleID,PtlInfo) -> (Either ParticleID (ParticleID,p), PtlInfo)
        tnodef (pid,pinfo) = (Left pid, pinfo)
        
        dnodef :: (ParticleID,(p,PtlInfo)) -> (Either ParticleID (ParticleID,p), PtlInfo)
        dnodef (pid,(prcid,pinfo)) = (Right (pid,prcid), pinfo)

-- | 

matchX :: (Functor m, Monad m, Show p) => 
          CrossID p  
       -> LHEvent 
       -> m (Either String (MatchedLHEventProcess p))
matchX (MkC {..}) ev@(LHEvent _einfo pinfos) = evalStateT (runErrorT m) pinfos
  where m = do let (in1,in2) = xincs
               mio <- getX (in1,in2,xouts)  
               return (getMatchedLHEvent xnode ev mio)

-- | 

matchD :: (Functor m, Monad m, Show p) => PDGID -> DecayID p -> LHEvent 
       -> m (Either String (MatchedLHEventProcess p))
matchD _ (MkT {..}) _ = return (Left "cannot match a process with terminal node")
matchD pdgid' (MkD {..}) ev@(LHEvent _einfo pinfos) = evalStateT (runErrorT m) pinfos 
  where m = do let procs = ptl_procs dnode  
                   ptlid' = ptl_ptlid dnode 
               case (lookupid pdgid' procs) of 
                 Nothing -> fail ("No such pdgid = " ++ show pdgid' ) 
                 Just (ProcPDG procid' _) -> do 
                   i' <- Left . (ptlid',)  <$> match1 In pdgid' 
                   os' <- mapM (checkD Out) douts 
                   rs' <- lift get 
                   let mio = (MIO [i'] os' rs')
                   return (getMatchedLHEvent procid' ev mio)
    


-- | 

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
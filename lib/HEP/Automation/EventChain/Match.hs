{-# LANGUAGE ScopedTypeVariables, RecordWildCards, TupleSections #-}

-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Automation.EventChain.Match
-- Copyright   : (c) 2012,2013 Ian-Woo Kim
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
import           HEP.Parser.LHE.Type
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
    




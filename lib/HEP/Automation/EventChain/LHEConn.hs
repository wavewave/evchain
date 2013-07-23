{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Automation.EventChain.LHEConn
-- Copyright   : (c) 2012,2013 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- Connecting multiple LHE files 
--
-----------------------------------------------------------------------------

module HEP.Automation.EventChain.LHEConn where 
-- other package of others
import           Control.Applicative ((<$>),(<*>))
import           Control.Monad.Error hiding (mapM)
import           Control.Monad.Identity (runIdentity,Identity(..))
import           Control.Monad.State hiding (mapM)
import           Data.Either
import           Data.Foldable (foldr,foldrM)
import           Data.Function (on)
import qualified Data.HashMap.Lazy as HM 
import qualified Data.IntMap as IM
import           Data.List (intercalate, sortBy)
import qualified Data.Map as M
import           Data.Traversable 
import           Data.Vector.Storable ((!))
import qualified Numeric.LinearAlgebra as NL
import           System.IO
-- other package of mine
import           HEP.Parser.LHE.Type
import           HEP.Util.Functions
-- this package
-- import           HEP.Automation.EventChain.Print
import           HEP.Automation.EventChain.Match
import           HEP.Automation.EventChain.Type.Match
import           HEP.Automation.EventChain.Type.Process
import           HEP.Automation.EventChain.Type.Skeleton
import           HEP.Automation.EventChain.Type.Spec
import           HEP.Automation.EventChain.Util
-- prelude
import           Prelude hiding (mapM,foldr)
import Debug.Trace

-- | 
type Status = Int

-- | 
getPDGID4ParticleID :: MatchedLHEventProcess p -> ParticleID -> Maybe PDGID
getPDGID4ParticleID ctxt ptl_id  
    | (not.null) filtered_in = Just . idup . snd . head $ filtered_in
    | (not.null) filtered_out = Just . idup . snd .  head $ filtered_out
    | otherwise = Nothing 
  where idtuple = (,) <$> either id fst . fst <*> snd  
        lst_in  = map idtuple . mlhev_incoming $ ctxt
        lst_out = map idtuple . mlhev_outgoing $ ctxt
        checkid = (== ptl_id) <$> fst
        filtered_in = filter checkid lst_in
        filtered_out = filter checkid lst_out

-- | 
chainProcIdx :: (Monad m) => 
                ProcessIndex 
             -> MatchedLHEventProcess p' 
             -> DecayID p 
             -> ErrorT String m ProcessIndex 
chainProcIdx pidx mev dcy  = do
    let ptl_id = foremostParticleID dcy
    case getPDGID4ParticleID mev ptl_id of 
      Nothing -> throwError "ParticleID not matched"
      Just pdg_id -> return ((ptl_id,pdg_id):pidx)

-- | 
matchFullDecay :: (Show p) =>
                  ContextEvent ProcessIndex    -- ^ current context for mother
               -> DecayID p 
               -> ProcessIndex  
               -> ErrorT String (State (ProcessMap [LHEvent])) (DecayFull ProcessIndex)
matchFullDecay ctxt (MkT _) ((iptl,ipdg):_) = return (MkT (iptl,ipdg))
matchFullDecay ctxt elem@(MkD dnode ds) pidx@((iptl,ipdg):_) = do 
    mevents <- HM.lookup pidx <$> lift get 
    case mevents of
      Nothing -> throwError (show pidx ++ " process doesn't exist")
      Just [] -> throwError (show pidx ++ " process has no more events")
      Just (lhe:lhes) -> do 
        mev0 <- (ErrorT . return . runIdentity) (matchD ipdg elem lhe)
        let mev = unMkMLHEPF . fmap (const pidx) . MkMLHEPF $ mev0 
            ptrip = findPTripletUsingPtlIDFrmOutPtls iptl momev 
            lxfrm = relLrntzXfrm ptrip momev
            momprocid = (mlhev_procinfo.selfEvent) ctxt 
            dctxt = CEvent (olxfrm NL.<> lxfrm) (Just (momprocid,ptrip)) mev
        modify (HM.adjust (const lhes) pidx) 
        mds <- mapM (\x->matchFullDecay dctxt x =<< chainProcIdx pidx mev x) ds
        return (MkD ((iptl,ipdg),dctxt) mds)
  where momev = selfEvent ctxt
        olxfrm = absoluteContext ctxt

-- | 
matchFullCross :: (Show p) => 
                  CrossID p 
               -> ErrorT String (State (ProcessMap [LHEvent])) 
                         (CrossFull ProcessIndex)
matchFullCross g@(MkC _ (inc1,inc2) outs) = do 
    mevents <- HM.lookup [] <$> get
    case mevents of 
      Nothing -> fail "root process doesn't exist"
      Just [] -> fail "no more root events"
      Just (lhe:lhes) -> do 
        mev0 <- (ErrorT . return . runIdentity) (matchX g lhe)
        let mev = unMkMLHEPF . fmap (const []) . MkMLHEPF $ mev0
        modify (HM.adjust (const lhes) [])           
        let xcontext = CEvent (NL.ident 4) Nothing mev  
        mi1 <- matchFullDecay xcontext inc1 =<< chainProcIdx [] mev inc1 
        mi2 <- matchFullDecay xcontext inc2 =<< chainProcIdx [] mev inc2
        mos <- mapM (\x -> matchFullDecay xcontext x =<< chainProcIdx [] mev x) outs 
        return (MkC xcontext (mi1,mi2) mos)


-- | 
adjustPtlInfosInMLHEvent :: ( PtlInfo -> PtlInfo
                            , (ParticleID,PtlInfo) -> PtlInfo) 
                         -> MatchedLHEventProcess ProcessIndex 
                         -> ParticleCoordMap 
                         -> ([PtlInfo],[PtlInfo],[PtlInfo],ParticleCoordMap)
adjustPtlInfosInMLHEvent (f,g) mev mm = (map snd inc,map snd out,int,mm'')
  where procid = mlhev_procinfo mev
        inc = map stripping (mlhev_incoming mev)
        out = map stripping (mlhev_outgoing mev)
        int = map f (mlhev_intermediate mev)
        insfunc x m = M.insert (procid,fst x) ((ptlid.snd) x) m
        mm' = foldr insfunc mm inc
        mm'' = foldr insfunc mm' out
        stripping =   ( (,) <$> fst <*> f . g )
                    . ( (,) <$> either id fst . fst <*> snd )



-- | 
getAdjustFunc4IDMom :: LorentzRotation 
                    -> PtlInfo 
                    -> (ProcessIndex,PTriplet) 
                    -> State (PtlID,Int,IM.IntMap PtlInfo,ParticleCoordMap) (PtlInfo -> PtlInfo,Int,IM.IntMap PtlInfo)
getAdjustFunc4IDMom lrot rpinfo (procid,PTriplet pid pcode opinfo) = do 
    (stid,stcol,rmap,stmm) <- get
    let oid = idChange stid (ptlid rpinfo)
        nid = maybe (error ("error in getAdjustFunc4IDMom: " ++ show (procid,pid) ++ "\n" ++ show stmm)) id (M.lookup (procid,pid) stmm)
        opinfo2 = maybe (error "error in opinfo in getAdjustFun4IDMom") id (IM.lookup nid rmap) 
        rmap1 = IM.adjust unstabilize nid rmap
        midadj = motherAdjustID (oid,nid) 
        (coloffset,colfunc) = colChangePair stcol (opinfo2,rpinfo)         
        idfunc = adjustIds (idChange stid) colfunc
    return (adjustMom lrot.adjustSpin (opinfo,rpinfo).midadj.idfunc,coloffset,rmap1) 


-- | 
accumTotalEvent :: CrossFull ProcessIndex -> LHEvent 
accumTotalEvent g =  
    let (_,_,result,_) = execState (traverse action . CrossF $ g) 
                                      (0,0, IM.empty :: IM.IntMap PtlInfo
                                      , M.empty :: ParticleCoordMap )
        result' = IM.elems result
        sortedResult = sortBy (compare `on` ptlid) result'
        evinfo = (mlhev_einfo . selfEvent . xnode) g 
        nptl = length sortedResult
        nevinfo = evinfo { nup = nptl } 
    in LHEvent nevinfo sortedResult 
  where action cev = do 
          let (lrot,mmom,mev) = (absoluteContext cev, relativeContext cev, selfEvent cev)
              pinfos = (getPInfos . mlhev_orig) mev
              ptlids = map ptlid pinfos
              icols = filter (/= 0) (concatMap ((\x -> [fst x, snd x]) . icolup ) pinfos)
              maxid = maximum ptlids 
              maxicol = maximum icols
              minicol = minimum icols
              deltaicol = if null icols then 0 else maxicol - minicol
 
          (stid,stcol,rmap,stmm) <- get
          let rpinfo = (snd . head . mlhev_incoming ) mev
          (change,coloffset,rmap1) <- maybe 
                                        (return (id,0,rmap))  
                                        (getAdjustFunc4IDMom lrot rpinfo) 
                                        mmom
          let (ri,ro,rm,stmm') = adjustPtlInfosInMLHEvent (change,snd) mev stmm
              kri = map ((,) <$> ptlid <*> id) ri
              kro = map ((,) <$> ptlid <*> id) ro
              krm = map ((,) <$> ptlid <*> id) rm 
              rmap2 = maybe (insertAll kri rmap1) (const rmap1) mmom 
              rmap3 = insertAll kro rmap2
              rmap4 = insertAll krm rmap3 
          put ( stid+maxid-1
              , stcol+deltaicol+1-coloffset 
              , rmap4
              , stmm')


-- | 
motherAdjustID :: (PtlID,PtlID) -> PtlInfo -> PtlInfo
motherAdjustID (oid,nid) = idAdj (\y -> if y == oid then nid else y)






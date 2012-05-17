{-# LANGUAGE ScopedTypeVariables #-}

module HEP.Automation.MadGraph.EventChain.LHEConn where 

import Control.Applicative ((<$>),(<*>))
import Control.Monad.State hiding (mapM)
import Data.Either
import Data.Foldable (foldr)
import Data.Function (on)
import qualified Data.IntMap as IM
import Data.List (intercalate, sortBy)
import qualified Data.Map as M
import Data.Traversable 
import Data.Vector.Storable ((!))

-- import Text.Printf
import System.IO
import qualified Data.Enumerator.List as EL
import qualified Numeric.LinearAlgebra as NL


import HEP.Parser.LHEParser.Type
import HEP.Util.Functions

import HEP.Automation.MadGraph.EventChain.Type 
import HEP.Automation.MadGraph.EventChain.Print

import Prelude hiding (mapM,foldr)

-- | 

type Status = Int

-- | 

type ParticleCoord = (ProcessID,ParticleID)

type ParticleCoordMap = M.Map ParticleCoord PtlID 

-- | 

findPtlIDStatus :: (PDGID,Status) -> [PtlInfo] -> (Maybe PtlInfo,[PtlInfo]) 
findPtlIDStatus (pdgid,st) pinfos = foldr f (Nothing,[]) pinfos 
  where f pinfo (Just r, rs) = (Just r,(pinfo:rs))
        f pinfo (Nothing, rs) = if (pdgid == idup pinfo) && (st == istup pinfo)
                                  then (Just pinfo,rs)
                                  else (Nothing,pinfo:rs)

-- | 

allFindPtlIDStatus :: [(ParticleID,PDGID,Status)] -> [PtlInfo] 
                      -> Either String ([(ParticleID,PtlInfo)],[PtlInfo])
allFindPtlIDStatus ls pinfos = foldr f (Right ([],pinfos)) ls
  where f _ (Left str) = Left str 
        f (pid,pdgid,status) (Right (done,remaining)) = 
          case findPtlIDStatus (pdgid,status) remaining of
            (Nothing,_) -> Left ((show pdgid) ++ "," ++ (show status) ++ " cannot be found.")
            (Just pinfo,remaining') -> Right ((pid,pinfo):done,remaining')


-- | 

mkIDTriple :: Status 
           -> DecayID 
           -> (ParticleID,PDGID,Status)
mkIDTriple st x = let (pid,pdgid) = getContent x in (pid,pdgid,st)

-- | 

matchInOut :: ProcessID 
           -> ([(ParticleID,PDGID,Status)],[(ParticleID,PDGID,Status)]) 
           -> LHEvent 
           -> Either String MatchedLHEvent
matchInOut procid (incids,outids) ev@(LHEvent einfo pinfos) = do 
    (matchinc,remaining) <- allFindPtlIDStatus incids pinfos 
    (matchout,remaining') <- allFindPtlIDStatus outids remaining
    return (MLHEvent procid ev einfo matchinc matchout remaining')

-- | 

matchPtl4Cross :: CrossID
               -> LHEvent
               -> Either String MatchedLHEvent 
matchPtl4Cross (GCross (XNode procid) inc out) lhe = matchInOut procid (incids,outids) lhe
  where incids = map (mkIDTriple (-1)) inc
        outids = map (mkIDTriple 1) out

-- | 

matchPtl4Decay :: (DNode (ParticleID,PDGID) ProcessID, [DecayID])
               -> LHEvent
               -> Either String MatchedLHEvent
matchPtl4Decay (inc,out) lhe = matchInOut procid (incids,outids) lhe 
  where (procid,incids) = case inc of 
                   DNode (x,y) p -> (p,[(x,y,-1)])
        outids = map (mkIDTriple 1) out 
   
-- | 

matchFullDecay :: Maybe MatchedLHEvent -- ^ context
               -> IM.IntMap LHEvent
               -> DecayID
               -> Either String DecayFull
matchFullDecay _ _ (GTerminal (TNode x)) = return (GTerminal (TNode x))
matchFullDecay c m (GDecay elem@(DNode (ptl_id,pdg_id) proc_id, ds)) = 
   case r of 
     Nothing -> Left $ show proc_id ++ " process doesn't exist"
     Just lhe -> do 
       mev <- matchPtl4Decay elem lhe
       mds <- mapM (matchFullDecay (Just mev) m) ds
       let newctxt = fmap (\y->(y,findParticleID (ptl_id,pdg_id) y)) c 
       return (GDecay (DNode (ptl_id,pdg_id) (CMLHEvent newctxt mev), mds)) 
  where r = IM.lookup proc_id m 
        findParticleID (i,pdg) y = let (_,after) = break (\x->fst x == i) (mlhev_outgoing y)  
                                   in (i,pdg,snd (head after))

-- | 

matchFullCross :: IM.IntMap LHEvent
               -> CrossID 
               -> Either String CrossFull
matchFullCross m g@(GCross (XNode pid) inc out) =
    case r of 
      Nothing -> Left $ show pid ++ " process doesn't exist"
      Just lhe -> do 
        (mev :: MatchedLHEvent) <- matchPtl4Cross g lhe
        (mis :: [DecayFull]) <- mapM (matchFullDecay (Just mev) m) inc
        (mos :: [DecayFull]) <- mapM (matchFullDecay (Just mev) m) out 
        let result :: CrossFull
            result = GCross (XNode (CMLHEvent Nothing mev)) mis mos 
        return result 
  where r = IM.lookup pid m 



-- | 

getPtlInfosFromCMLHEvent :: (PtlInfo -> PtlInfo, (ParticleID,PtlInfo) -> PtlInfo) 
                         -> ContextMatchedLHEvent 
                         -> ParticleCoordMap 
                         -> ([PtlInfo],[PtlInfo],[PtlInfo],ParticleCoordMap)
getPtlInfosFromCMLHEvent (f,g) (CMLHEvent u c) mm = (map snd inc,map snd out,int,mm'')
  where procid = mlhev_procid c
        inc = map ((,) <$> fst <*> (f.g)) (mlhev_incoming c)
        out = map ((,) <$> fst <*> (f.g)) (mlhev_outgoing c)
        int = map f (mlhev_intermediate c)
        insfunc x m = M.insert (procid,fst x) ((ptlid.snd) x) m
        mm' = foldr insfunc mm inc
        mm'' = foldr insfunc mm' out

-- | 

accumTotalEvent :: CrossFull -> IO [PtlInfo]
accumTotalEvent g = do (_,_,result,_) <- execStateT (traverse action g) (0,0,[],M.empty :: ParticleCoordMap) 
                       let sortedResult = sortBy (compare `on` ptlid) result
                       putStrLn "============!!!=============="
                      
                       putStrLn $ pformats sortedResult
                       
                       return sortedResult
  where action cmlhev = do 
          let pinfos = getPInfos . mlhev_orig . current $ cmlhev
              ptlids = map ptlid pinfos
              icols = filter (/= 0) (concatMap ((\x -> [fst x, snd x]) . icolup )
                                               pinfos)
              maxid = maximum ptlids 
              maxicol = maximum icols
              minicol = minimum icols 
          (stid,stcol,rlst,stmm) <- get
          let idfunc = adjustIds (idChange stid) (colChange stcol)

          momf <-case upper cmlhev of 
                   Nothing -> return id
                   Just (ev,(pid,pcode,opinfo)) -> liftIO $ do  
                       putStrLn "Mother=" 
                       putStrLn $ pformat opinfo  
                       let rpinfo = (snd . head . mlhev_incoming . current ) cmlhev
                       putStrLn $ pformat rpinfo 
                       let oid = idChange stid (ptlid rpinfo)
                           midadj = motherAdjustID oid (mlhev_procid ev,pid) stmm
                       return $ (adjustMomSpin (opinfo,rpinfo) . midadj)
                                                     
                       -- momfunc = adjustIdMomSpin (n1,rn1) . adjustFirst (ptlid n1) $ npinfos2' 

          let (ri,ro,rm,stmm') = getPtlInfosFromCMLHEvent (momf.idfunc,snd) cmlhev stmm
              rlst' = rlst ++ maybe ri (const []) (upper cmlhev) ++ ro ++ rm  
                       
          liftIO $ putStrLn (pformats ri) 
          liftIO $ putStrLn (pformats ro)
          liftIO $ putStrLn (pformats rm)
          liftIO $ putStrLn (show stmm')
          liftIO $ putStrLn "**"
          put (maxid-1,stcol+maxicol-minicol+1,rlst',stmm')

-- | 

motherAdjustID :: PtlID -> ParticleCoord -> ParticleCoordMap -> PtlInfo -> PtlInfo
motherAdjustID oid pc pcm = idAdj f
  where f = maybe id (\nid y -> if y == oid then nid else y)  (M.lookup pc pcm) 



-- | 

countPtls :: CrossFull -> Int
countPtls g = execState (traverse action g) 0  
  where action x = 
          modify (\y -> y + length (mlhev_incoming (current x))
                          + length (mlhev_outgoing (current x))
                          + length (mlhev_intermediate (current x)))


-- | 

accumLHEvent :: CrossFull -> [PtlInfo]
accumLHEvent g = execState (traverse action g) [] 
  where action x = 
          modify (\acc -> acc ++ (map snd (mlhev_incoming (current x)))
                              ++ (map snd (mlhev_outgoing (current x)))
                              ++ mlhev_intermediate (current x))

-- | 

checkHigh :: CrossFull -> IO ()
checkHigh g = do execStateT (traverse action g) (([],[]) :: ([(ParticleID,PtlID)],[(ParticleID,PtlID)]) )
                 return () 
  where action x = do 
          st <- get  
          liftIO $ putStrLn "-*-*-*-"
          liftIO $ putStrLn $ show st
          liftIO $ putStrLn $ "length of incoming = " ++ (show (length (mlhev_incoming (current x))))
          put (extractIDsFromMLHE (current x)) 
-- | 

extractIDsFromMLHE :: MatchedLHEvent -> ([(ParticleID,PtlID)],[(ParticleID,PtlID)])
extractIDsFromMLHE mlhe = 
  ( map (\x->(fst x, ptlid (snd x))) (mlhev_incoming mlhe)
  , map (\x->(fst x, ptlid (snd x))) (mlhev_outgoing mlhe) )

-- | 

getN1 :: [PtlInfo] -> ([PtlInfo],[PtlInfo])
getN1 = break (\x -> idup x == 1000022 && istup x == 1)    


-- | 

idChange :: Int -> Int -> Int
idChange r a = a + r

-- | 

colChange :: Int -> Int -> Int 
colChange r 0 = 0 
colChange r a = a + r 

-- | 

adjustIds :: (Int -> Int) -> (Int->Int) -> PtlInfo -> PtlInfo
adjustIds idmap icolmap pinfo = 
    pinfo { ptlid = idmap optlid
          , mothup = (idmap omothup_x, idmap omothup_y) 
          , icolup = (icolmap oicolup_x, icolmap oicolup_y)
          } 
  where optlid = ptlid pinfo 
        (omothup_x, omothup_y) = mothup pinfo
        (oicolup_x, oicolup_y) = icolup pinfo 
         

-- | 

adjustFirst :: Int -> [PtlInfo] -> [PtlInfo]
adjustFirst n (x:xs) = x { mothup = (n,n), istup = 2 } : xs 

-- | 

boostBack :: FourMomentum -> PtlInfo -> PtlInfo 
boostBack mom pinfo = 
  let v3 = NL.scale (-1) .  beta . fourMomentumToLorentzVector $ mom
      lrot = boost v3  
      v = lrot NL.<> fourMomentumToLorentzVector (pupTo4mom (pup pinfo))
      e = v ! 0
      px = v ! 1 
      py = v ! 2 
      pz = v ! 3 
      masssqr = e*e-px*px-py*py-pz*pz
      mass = if masssqr < 0 then 0 else if masssqr < 1e-3 then 0 else sqrt masssqr
      npup = (px,py,pz,e,mass)
  in pinfo { pup = npup }
      
-- | 

spinAdj :: Double -> PtlInfo -> PtlInfo 
spinAdj spn pinfo = pinfo { spinup = spn * spinup pinfo } 

-- | 

idAdj :: (Int -> Int) -> PtlInfo -> PtlInfo 
idAdj idfunc pinfo = pinfo { mothup = (idfunc mid1, idfunc mid1 {- mid2 -} ) } 
                     -- because of bug in madgraph 
  where (mid1,_mid2) = mothup pinfo

-- | 

adjustMomSpin :: (PtlInfo,PtlInfo) -> PtlInfo -> PtlInfo
adjustMomSpin (opinfo,rpinfo) = spinAdj spn . boostBack mom
  where mom = pupTo4mom . pup $ opinfo 
        spn = spinup opinfo * spinup rpinfo 


-- | 

adjustIdMomSpin :: (PtlInfo,PtlInfo)-> [PtlInfo] -> [PtlInfo]
adjustIdMomSpin (opinfo,rpinfo) = map (idAdj idfunc . spinAdj spn . boostBack mom) 
  where mom = pupTo4mom . pup $ opinfo 
        spn = spinup opinfo * spinup rpinfo 
        ido = ptlid opinfo 
        idr = ptlid rpinfo 
        idfunc x = if x == idr then ido else x

-- | 

interwine3 :: Handle -> Maybe (LHEvent,a,b) -> Maybe (LHEvent,a,b) -> Maybe (LHEvent,a,b)
              -> IO ()
interwine3 h (Just (lhe1,_,_)) 
             (Just (lhe2,_,_)) 
             (Just (lhe3,_,_)) = do 
  let lhe1' = interwine2 lhe1 lhe2
      lhe1_final = interwine2 lhe1' lhe3
  hPutStrLn h (lheFormatOutput lhe1_final)

-- | 

interwine2 :: LHEvent -> LHEvent -> LHEvent 
interwine2 (LHEvent einfo1 pinfos1) (LHEvent einfo2 pinfos2) =  
  let ptlids1 = map ptlid pinfos1
      icols1 = filter (/= 0) (concatMap ((\x -> [fst x, snd x]) . icolup )
                                pinfos1)
      maxid1 = maximum ptlids1 
      maxicol1 = maximum icols1 
      minicol1 = minimum icols1 
      npinfos2'  = map (adjustIds (idChange (maxid1-1)) (colChange (maxicol1-minicol1+1)))
                       pinfos2
      (first1,(n1:rest1)) = getN1 pinfos1
      pinfos1' = first1 ++ (n1 { istup = 2} : rest1)
      rn1  = head $ npinfos2' 
      (_:npinfos2) = adjustIdMomSpin (n1,rn1) . adjustFirst (ptlid n1) $ npinfos2'  
      npinfos = pinfos1' ++ npinfos2
      numptls = length npinfos
      neinfo = einfo1 { nup = numptls }
  in LHEvent neinfo npinfos 


-- | 

chainDecay :: CrossFull -> [PtlInfo] 
chainDecay = foldr f [] 
  where f e@(CMLHEvent u c) acc = let (LHEvent _einfo pinfos) = mlhev_orig c
                                  in pinfos ++ acc 

-- getPtlInfosSortedFromCMLHEvent e ++ acc 
  {- map snd (mlhev_incoming c)
     ++ map snd (mlhev_outgoing c)
     ++ mlhev_intermediate c -}

 
getPtlInfosSortedFromCMLHEvent :: ContextMatchedLHEvent -> [PtlInfo]
getPtlInfosSortedFromCMLHEvent (CMLHEvent u c) =  sortBy (compare `on` ptlid) (inc ++ out ++ int)
  where inc = map snd (mlhev_incoming c)
        out = map snd (mlhev_outgoing c)
        int = mlhev_intermediate c

{-
traverseWithAC :: CrossFull -> IO ()
traverseWithAC g = do execStateT (traverse action g) 0 
-}

{-
connectEvent :: ContextMatchedLHEvent -> [PtlInfo] -> [PtlInfo]
connectEvent e@(CMLHEvent u c) acc = 
  let pinfos2 = getPtlInfosSortedFromCMLHEvent e
      ptlids1 = map ptlid acc
      icols1 = filter (/= 0) (concatMap ((\x -> [fst x, snd x]) . icolup )
                                acc)
      maxid1 = maximum ptlids1
      maxicol1 = maximum icols1 
      minicol1 = minimum icols1 
      npinfos2'  = map (adjustIds (idChange (maxid1-1)) (colChange (maxicol1-minicol1+1)))
                       pinfos2
      (first1,(n1:rest1)) = getN1 pinfos1
      pinfos1' = first1 ++ (n1 { istup = 2} : rest1)
      rn1  = head $ npinfos2' 
      (_:npinfos2) = adjustIdMomSpin (n1,rn1) . adjustFirst (ptlid n1) $ npinfos2'  
      npinfos = pinfos1' ++ npinfos2
      numptls = length npinfos
      neinfo = einfo1 { nup = numptls }
  in LHEvent neinfo npinfos 

-}

{-

-- | 

accumLHEventFromDecay :: [PtlInfo] -> DecayFull -> [PtlInfo]
accumLHEventFromDecay (GTerminal ) = foldr f [] 
  where f  acc
-}

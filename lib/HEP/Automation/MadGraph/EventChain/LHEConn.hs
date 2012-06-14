{-# LANGUAGE ScopedTypeVariables #-}

module HEP.Automation.MadGraph.EventChain.LHEConn where 

import Control.Applicative ((<$>),(<*>))
import Control.Monad.State hiding (mapM)
import Data.Either
import Data.Foldable (foldr,foldrM)
import Data.Function (on)
import qualified Data.IntMap as IM
import Data.List (intercalate, sortBy)
import qualified Data.Map as M
import Data.Traversable 
import Data.Vector.Storable ((!))

-- import Text.Printf
import System.IO
-- import qualified Data.Conduit.List as CL
import qualified Numeric.LinearAlgebra as NL


import HEP.Parser.LHEParser.Type
import HEP.Util.Functions

import HEP.Automation.MadGraph.EventChain.Type 
import HEP.Automation.MadGraph.EventChain.Print

import Prelude hiding (mapM,foldr)

-- | 

type Status = Int

-- | 

statusIn :: Status
statusIn = -1 

-- | 

statusOut :: Status
statusOut = 1

-- | 

statusIntermediate :: Status
statusIntermediate = 2

-- | 

type ParticleCoord = (ProcessID,ParticleID)

type ParticleCoordMap = M.Map ParticleCoord PtlID 


{- 
-- | find a particle with a given PDGID and Status (final, intermediate, initial) 

findPtlIDStatus :: (PDGID,Status) -> [PtlInfo] -> (Maybe PtlInfo,[PtlInfo]) 
findPtlIDStatus (pdgid,st) pinfos = foldr f (Nothing,[]) pinfos 
  where f pinfo (Just r, rs) = (Just r,(pinfo:rs))
        f pinfo (Nothing, rs) = if (pdgid == idup pinfo) && (st == istup pinfo)
                                  then (Just pinfo,rs)
                                  else (Nothing,pinfo:rs)

-- | find all particles with a given PDGID and Status

allFindPtlIDStatus :: [(ParticleID,PDGID,Status)] -> [PtlInfo] 
                      -> Either String ([(ParticleID,PtlInfo)],[PtlInfo])
allFindPtlIDStatus ls pinfos = foldr f (Right ([],pinfos)) ls
  where f _ (Left str) = Left str 
        f (pid,pdgid,status) (Right (done,remaining)) = 
          case findPtlIDStatus (pdgid,status) remaining of
            (Nothing,_) -> Left ((show pdgid) ++ "," ++ (show status) ++ " cannot be found.")
            (Just pinfo,remaining') -> Right ((pid,pinfo):done,remaining')


-}

-- | match function type to find a particle with a given pdgid and status criterion

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
                   -> Either String ([(ParticleID,PtlInfo)],[PtlInfo]) -- ^ error monad, returning matched with ParticleId and unmatched (no id) 
matchAllPtlWSelect ls pinfos = foldrM f ([],pinfos) ls
  where f (pid,sel) (done,remaining) = 
          case findPtlWSelect sel remaining of
            (Nothing,_)             -> Left ((description sel) ++ " cannot be found.")
            (Just pinfo,remaining') -> return ((pid,pinfo):done,remaining')


-- | match incoming and outgoing particles with a given id set from a given event file

matchInOut :: ProcessID 
           -> ([(ParticleID,SelectFunc)],[(ParticleID,SelectFunc)]) 
           -> LHEvent 
           -> Either String MatchedLHEvent
matchInOut procid (incids,outids) ev@(LHEvent einfo pinfos) = do 
    (matched_inc,remaining)  <- matchAllPtlWSelect incids pinfos
    (matched_out,remaining') <- matchAllPtlWSelect outids remaining
    return (MLHEvent procid ev einfo matched_inc matched_out remaining')

-- | 

data InOutDir = In | Out 
              deriving Show 

-- | 

matchPtl4Cross :: CrossID
               -> LHEvent
               -> Either String MatchedLHEvent 
matchPtl4Cross (GCross (XNode procid) inc out) lhe = matchInOut procid (incids,outids) lhe
  where incids = map (getSelPair In)  inc
        outids = map (getSelPair Out) out 

-- |

mkSelFunc :: InOutDir -> PKind -> SelectFunc
mkSelFunc dir pkind = let f (pdgid',st) = isMatchedStatus st && isMatchedKind pkind pdgid'
                      in SelectFunc f ("(pkind = " ++ show pkind ++ "," ++ show dir ++ ")")
  where isMatchedStatus st = case dir of 
                               In -> st == statusIn 
                               Out -> st == statusOut
        isMatchedKind (KPDGID pdg_id) pdg_id' = pdg_id == pdg_id'
        isMatchedKind (PtlPtlbar pdg_id) pdg_id' = pdg_id == pdg_id' || pdg_id == (-pdg_id')
        isMatchedKind MultiJet pdg_id' = pdg_id' == 1 || pdg_id' == (-1)
                                         || pdg_id' == 2 || pdg_id' == (-2)
                                         || pdg_id' == 3 || pdg_id' == (-3)
                                         || pdg_id' == 4 || pdg_id' == (-4)
                                         || pdg_id' == 21 
        
-- | 

getSelPair :: InOutDir -> DecayID -> (ParticleID,SelectFunc)
getSelPair dir x = let (pid,pkind) = getContent x  
                   in (pid,mkSelFunc dir pkind)

-- | 

matchPtl4Decay :: (DNode (ParticleID,PKind) ProcessID, [DecayID])
               -> LHEvent
               -> Either String MatchedLHEvent
matchPtl4Decay (inc,out) lhe = matchInOut procid (incids,outids) lhe 
  where procid :: ProcessID
        incids :: [(ParticleID,SelectFunc)]
        (procid,incids) = case inc of DNode (x,y) p -> (p,[(x,mkSelFunc In y)])
        outids = map (getSelPair Out) out 

-- | 

matchFullDecay :: Maybe MatchedLHEvent -- ^ context
               -> IM.IntMap LHEvent
               -> DecayID
               -> Either String DecayFull
matchFullDecay c m (GTerminal (TNode (ptl_id,pkind))) = 
    case pkind of 
      KPDGID pdg_id -> return (GTerminal (TNode (ptl_id,pdg_id)))
      _ -> return (GTerminal (TNode (ptl_id,0)))   -- this is very bad but I do not have any solution.
matchFullDecay c m (GDecay elem@(DNode (ptl_id,pkind) proc_id, ds)) = 
    case pkind of 
      KPDGID pdg_id -> case r of 
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

insertAll :: [(Int,PtlInfo)] -> IM.IntMap PtlInfo -> IM.IntMap PtlInfo
insertAll lst m = foldr f m lst where f (k,a) m = IM.insert k a m

-- | 

accumTotalEvent :: CrossFull -> IO [PtlInfo]
accumTotalEvent g = do (_,_,result,_) <- execStateT (traverse action g) 
                                                    (0,0, IM.empty :: IM.IntMap PtlInfo
                                                        , M.empty :: ParticleCoordMap ) 
                       let result' = IM.elems result
                       let sortedResult = sortBy (compare `on` ptlid) result'
                       -- putStrLn "============!!!=============="
                       -- putStrLn $ pformats sortedResult
                       return sortedResult 
  where action cmlhev = do 
          let pinfos = getPInfos . mlhev_orig . current $ cmlhev
              ptlids = map ptlid pinfos
              icols = filter (/= 0) (concatMap ((\x -> [fst x, snd x]) . icolup )
                                               pinfos)
              maxid = maximum ptlids 
              maxicol = maximum icols
              minicol = minimum icols 
          (stid,stcol,rmap,stmm) <- get
          let mopinfo = fmap (\(_,(_,_,x))->x) (upper cmlhev)
              rpinfo = (snd . head . mlhev_incoming . current ) cmlhev
              (coloffset,colfunc) = colChangePair stcol (mopinfo,rpinfo) 
          let idfunc = adjustIds (idChange stid) colfunc

          (momf,rmap1) <- case upper cmlhev of 
            Nothing -> return (id,rmap)
            Just (ev,(pid,pcode,opinfo)) -> liftIO $ do  
              -- putStrLn "Mother=" 
              -- putStrLn $ pformat opinfo  
              let rpinfo = (snd . head . mlhev_incoming . current ) cmlhev
              -- putStrLn $ pformat rpinfo 
              let oid = idChange stid (ptlid rpinfo)
                  nid = case M.lookup (mlhev_procid ev,pid) stmm of
                          Nothing -> error " herehere " 
                          Just n -> n 
                  unstabilize pinfo = pinfo { istup = 2 } 
                  rmap1 = IM.adjust unstabilize nid rmap
                  midadj = motherAdjustID (oid,nid) 
              return $ (adjustMomSpin (opinfo,rpinfo) . midadj , rmap1)
          let (ri,ro,rm,stmm') = getPtlInfosFromCMLHEvent (momf.idfunc,snd) cmlhev stmm
              kri = map ((,) <$> ptlid <*> id) ri
              kro = map ((,) <$> ptlid <*> id) ro
              krm = map ((,) <$> ptlid <*> id) rm 
              rmap2 = maybe (insertAll kri rmap1) (const rmap1) (upper cmlhev)
              rmap3 = insertAll kro rmap2
              rmap4 = insertAll krm rmap3 

                       
          -- liftIO $ putStrLn (pformats ri) 
          -- liftIO $ putStrLn (pformats ro)
          -- liftIO $ putStrLn (pformats rm)
          -- liftIO $ putStrLn (show stmm')
          -- liftIO $ putStrLn $ concat (IM.elems (fmap pformat rmap4 ))
          -- liftIO $ putStrLn "**"
          put (stid+maxid-1,stcol+maxicol-minicol+1-coloffset,rmap4,stmm')

-- | 

motherAdjustID :: (PtlID,PtlID) -> PtlInfo -> PtlInfo
motherAdjustID (oid,nid) = idAdj (\y -> if y == oid then nid else y)


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

-- | obsolete

colChange :: Int -> Int -> Int 
colChange r 0 = 0 
colChange r a = a + r 

-- |

colChangeFunc :: Int -> (Maybe PtlInfo,PtlInfo) -> Int -> Int 
colChangeFunc offset (moptl,nptl) 0 = 0 
colChangeFunc offset (Nothing,nptl) a =  if a == col1 || a == col2 then a else a + offset  
  where (col1,col2) = icolup nptl 
colChangeFunc offset (Just optl,nptl) a 
  | a == col1 = fst (icolup optl)
  | a == col2 = snd (icolup optl) 
  | otherwise = a + offset 
  where (col1,col2) = icolup nptl 


-- | 

colChangeOffset :: (Maybe PtlInfo,PtlInfo) -> Int 
colChangeOffset (moptl,nptl) = let r | col1 /=0 && col2 /= 0 = 2
                                     | col1 /=0 && col2 == 0 = 1 
                                     | otherwise = 0 
                               in r 
  where (col1,col2) = icolup nptl 

colChangePair :: Int -> (Maybe PtlInfo,PtlInfo) -> (Int, Int -> Int)
colChangePair offset ptls = (colChangeOffset ptls, colChangeFunc offset ptls)


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

-- |
 
getPtlInfosSortedFromCMLHEvent :: ContextMatchedLHEvent -> [PtlInfo]
getPtlInfosSortedFromCMLHEvent (CMLHEvent u c) =  sortBy (compare `on` ptlid) (inc ++ out ++ int)
  where inc = map snd (mlhev_incoming c)
        out = map snd (mlhev_outgoing c)
        int = mlhev_intermediate c

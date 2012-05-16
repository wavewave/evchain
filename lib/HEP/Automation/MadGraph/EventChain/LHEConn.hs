module HEP.Automation.MadGraph.EventChain.LHEConn where 

import HEP.Parser.LHEParser.Type
import Data.Either
import Data.List
import Text.Printf
import System.IO
import qualified Data.Enumerator.List as EL
import qualified Numeric.LinearAlgebra as NL
import HEP.Util.Functions
import Data.Vector.Storable ((!))

import HEP.Automation.MadGraph.EventChain.Type 
import qualified Data.IntMap as M


-- | 

-- data DecayNodePair = DNodeId (PtlID,PDGID)

-- | 

-- | 

-- type TaggedDecayNode = Either DecayNode DecayNodePair

-- | 

type Status = Int

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
           -> DecayID -- GDecayTop DNode TNode (ParticleID,PDGID) 
           -> (ParticleID,PDGID,Status)
mkIDTriple st x = let (pid,pdgid) = getContent x in (pid,pdgid,st)

-- | 

matchInOut :: ([(ParticleID,PDGID,Status)],[(ParticleID,PDGID,Status)]) 
           -> LHEvent 
           -> Either String MatchedLHEvent
              -- ([(ParticleID,PtlInfo)],[(ParticleID,PtlInfo)],[PtlInfo])
matchInOut (incids,outids) (LHEvent einfo pinfos) = do 
    (matchinc,remaining) <- allFindPtlIDStatus incids pinfos 
    (matchout,remaining') <- allFindPtlIDStatus outids remaining
    return (MLHEvent einfo matchinc matchout remaining')

-- | 

matchPtl4Cross :: CrossID -- GCross XNode DNode TNode (ParticleID,PDGID)
               -> LHEvent
               -> Either String MatchedLHEvent 
matchPtl4Cross (GCross inc out xpr) lhe = matchInOut (incids,outids) lhe
  where incids = map (mkIDTriple (-1)) inc
        outids = map (mkIDTriple 1) out

-- | 

matchPtl4Decay :: (DNode (ParticleID,PDGID) ProcessID, [DecayID])
               -- [GDecayTop DNode TNode (ParticleID,PDGID)]
               -> LHEvent
               -> Either String MatchedLHEvent
matchPtl4Decay (inc,out) lhe = matchInOut (incids,outids) lhe 
  where incids = case inc of 
                   DNode (x,y) _ -> [(x,y,-1)]
        outids = map (mkIDTriple 1) out 
  
-- | 

{-
matchFullDecay :: DecayID
               -> M.IntMap LHEvent
               -> Either String DecayFull
matchFullDecay (GTerminal (TNode x)) _ = return (GTerminal (TNode (TerminalParticle))
-}
  

-- | 
{-
matchFullCross :: GCross XNode DNode TNode (ParticleID,PDGID) 
               -> M.IntMap LHEvent
               -> Either String (GCross XNode DNode TNode MatchedLHEvent)
matchFullCross g@(GCross inc out proc) m =
    mapM (\x -> M.lookup x m) ps 
  where ps = getProcessIDFromCross g 
-}


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

lheFormatOutput :: LHEvent -> String 
lheFormatOutput (LHEvent einfo pinfos) =
  "<event>" ++ endl 
  ++ printf "%2d" (nup einfo) -- ++ "  " 
  ++ printf "%4d" (idprup einfo) ++ " " 
  ++ printf "%14.7E" (xwgtup einfo) ++ " " 
  ++ printf "%14.7E" (scalup einfo) ++ " " 
  ++ printf "%14.7E" (aqedup einfo) ++ " " 
  ++ printf "%14.7E" (aqcdup einfo) ++ endl 
  ++ concatMap pformat pinfos 
  ++ "</event>" -- ++ endl

-- | 

pformat :: PtlInfo -> String 
pformat pinfo = 
    printf "%9d" (idup  pinfo)
    ++ printf "%5d" (istup pinfo)
    ++ printf "%5d" (fst (mothup pinfo))
    ++ printf "%5d" (snd (mothup pinfo))
    ++ printf "%5d" (fst (icolup pinfo))
    ++ printf "%5d" (snd (icolup pinfo))
    ++ printf "%19.11E" pupx
    ++ printf "%19.11E" pupy
    ++ printf "%19.11E" pupz
    ++ printf "%19.11E" pupt 
    ++ printf "%19.11E" pupm 
    ++ printf "%4.1f" (vtimup pinfo)
    ++ printf "%5.1f" (spinup pinfo)
    ++ endl 
  where (pupx,pupy,pupz,pupt,pupm) = pup pinfo 


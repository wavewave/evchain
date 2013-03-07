{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Automation.MadGraph.EventChain.Util
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- Utility functions for evchain
--
-----------------------------------------------------------------------------

module HEP.Automation.EventChain.Util where

-- from others
import           Control.Applicative ((<$>))
import           Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.IntMap as IM
import           Data.Vector.Storable ((!))
import           Numeric.LinearAlgebra (scale,(<>),(<.>))
import           System.IO
-- from hep-platfrom
import           HEP.Parser.LHE.Type 
import           HEP.Util.Count 
import           HEP.Util.Functions
-- from this package
import           HEP.Automation.EventChain.Type.Match
import           HEP.Automation.EventChain.Type.Skeleton
import           HEP.Automation.EventChain.Type.Spec
-- 
import Debug.Trace


-- | 
insertAll :: [(Int,PtlInfo)] -> IM.IntMap PtlInfo -> IM.IntMap PtlInfo
insertAll lst m = foldr f m lst where f (k,a) m = IM.insert k a m

-- | 
idChange :: Int -> Int -> Int
idChange r a = a + r

-- |
colChangeFunc :: Int -> (PtlInfo,PtlInfo) -> Int -> Int 
colChangeFunc offset  _ 0 = 0 
colChangeFunc offset (optl,nptl) a 
    | a == col1 = fst (icolup optl) 
    | a == col2 = snd (icolup optl) 
    | otherwise = a + offset  
  where (col1,col2) = icolup nptl 

-- colChangeFunc offset (Nothing,nptl) a = a  


-- | 
colChangeDeltaOffset :: PtlInfo -> Int  
colChangeDeltaOffset nptl = 
    let r | col1 /=0 && col2 /= 0 = 2 
          | col1 /=0 && col2 == 0 = 1
          | col1 ==0 && col2 /= 0 = 1  
          | otherwise = 0  
    in r 
  where (col1,col2) = icolup nptl 

 -- (Maybe PtlInfo,PtlInfo) -> Int 
-- colChangeOffset (Nothing,_) = 0

-- | 
colChangePair :: Int 
              -> (PtlInfo,PtlInfo) 
              -> (Int, Int -> Int)
colChangePair offset (optl,nptl) = 
    (colChangeDeltaOffset nptl, colChangeFunc offset (optl,nptl))
  

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
boostBackXfrm :: FourMomentum -> LorentzRotation 
boostBackXfrm mom = let v3 = scale (-1) .  beta . fourMomentumToLorentzVector $ mom
                    in boost v3  

-- | 
boostBack :: FourMomentum -> PtlInfo -> PtlInfo 
boostBack mom pinfo = 
  let lrot = boostBackXfrm mom
      v = lrot <> fourMomentumToLorentzVector (pupTo4mom (pup pinfo))
      e = v ! 0
      px = v ! 1 
      py = v ! 2 
      pz = v ! 3 
      masssqr = e*e-px*px-py*py-pz*pz
      mass = if masssqr < 0 then 0 else if masssqr < 1e-3 then 0 else sqrt masssqr
      npup = (px,py,pz,e,mass)
  in pinfo { pup = npup }

-- | 
idAdj :: (Int -> Int) -> PtlInfo -> PtlInfo 
idAdj idfunc pinfo = pinfo { mothup = (idfunc mid1, idfunc mid1 {- mid2 -} ) } 
                     -- because of bug in madgraph 
  where (mid1,_mid2) = mothup pinfo

-- |
adjustSpin :: (PtlInfo,PtlInfo) -> PtlInfo -> PtlInfo 
adjustSpin (opinfo,rpinfo) pinfo = pinfo { spinup = spn * spinup pinfo }
  where spn = spinup opinfo * spinup rpinfo 

-- | 
adjustMom :: LorentzRotation -> PtlInfo -> PtlInfo
adjustMom lrot pinfo = let v = lrot <> fourMomentumToLorentzVector (pupTo4mom (pup pinfo))
                           e = v ! 0
                           px = v ! 1 
                           py = v ! 2 
                           pz = v ! 3 
                           masssqr = e*e-px*px-py*py-pz*pz
                           mass = if masssqr < 0 then 0 else if masssqr < 1e-3 then 0 else sqrt masssqr
                           npup = (px,py,pz,e,mass)
                       in pinfo { pup = npup }
             
-- | 
unstabilize :: PtlInfo -> PtlInfo 
unstabilize pinfo = pinfo { istup = 2 }

-- | 
flipMaybe :: Maybe a -> b -> (a -> b) -> b 
flipMaybe m f s = maybe f s m

-- | 
relLrntzXfrm :: PTriplet
             -> MatchedLHEventProcess p -- ^ mother event in mother rest frame
             -> LorentzRotation 
relLrntzXfrm ptrip mother = 
    let mompinfo = pt_pinfo ptrip 
        mommom = pupTo4mom . pup $ mompinfo 
    in boostBackXfrm mommom



-- | 
findPTripletUsingPtlIDFrmOutPtls :: ParticleID -> MatchedLHEventProcess p -> PTriplet
findPTripletUsingPtlIDFrmOutPtls i ev = 
    let (_,after) = break ((== i) <$> (either id fst . fst)) (mlhev_outgoing ev)
        pinfo = snd (head after)
        pdg = idup pinfo 
    in PTriplet i pdg pinfo 




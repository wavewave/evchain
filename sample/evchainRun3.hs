{-# LANGUAGE ScopedTypeVariables, NoMonomorphismRestriction, RecordWildCards #-}

module Main where

import           Control.Applicative
import qualified Data.HashMap.Lazy as HM
import qualified Data.Map as M
import           System.Directory
import           System.FilePath ((</>))
import           System.Log.Logger
-- 
import HEP.Automation.EventChain.Driver 
import HEP.Automation.EventChain.Type.MultiProcess
import HEP.Automation.EventChain.Type.Spec
import HEP.Automation.EventChain.Type.Process
import HEP.Automation.EventChain.SpecDSL
import HEP.Automation.MadGraph.Model
import HEP.Automation.MadGraph.Model.SM
import HEP.Automation.MadGraph.SetupType
import HEP.Automation.MadGraph.Type
-- 
import qualified Paths_madgraph_auto as PMadGraph 
import qualified Paths_madgraph_auto_model as PModel 

top :: [Int]
top = [6]

antitop :: [Int]
antitop = [-6]

bottom :: [Int]
bottom = [5]

antibottom :: [Int]
antibottom = [-5]

wplus :: [Int]
wplus = [24]

wminus :: [Int]
wminus = [-24]

z :: [Int] 
z = [23]

wpm :: [Int]
wpm = wplus ++ wminus

lepton12 :: [Int]
lepton12 = [11,13]

antilepton12 :: [Int]
antilepton12 = [-11,-13] 

lepton_antilepton12 :: [Int]
lepton_antilepton12 = lepton12 ++ antilepton12

neutrino12 :: [Int]
neutrino12 = [12,14]

antineutrino12 :: [Int]
antineutrino12 = [-12,-14] 

neutrino_antineutrino12 :: [Int]
neutrino_antineutrino12 = neutrino12 ++ antineutrino12

all_lepton_neutrino12 :: [Int]
all_lepton_neutrino12 = lepton_antilepton12 ++ neutrino_antineutrino12

p_top :: DDecay
p_top = d (top, [t wplus, t bottom])

p_antitop :: DDecay
p_antitop = d (antitop, [t wminus, t antibottom])

p_zz :: DCross 
p_zz = x (t proton, t proton, [p_z, p_z])

p_wz :: DCross
p_wz = x (t proton, t proton, [p_w, p_z])

p_ww :: DCross
p_ww = x (t proton, t proton, [p_w, p_w])

p_z :: DDecay
p_z = d (z, [t all_lepton_neutrino12, t all_lepton_neutrino12])

p_w :: DDecay
p_w = d (wpm, [t lepton_antilepton12, t neutrino_antineutrino12] )

map_zz :: ProcSpecMap
map_zz = 
    HM.fromList [ (Nothing       , MGProc [] [ "p p > z z" ])
                , (Just (3,23,[]), MGProc [] [ "z > l+ l- "
                                             , "z > ve ve~"
                                             , "z > vm vm~"] ) 
                , (Just (4,23,[]), MGProc [] [ "z > l+ l- "
                                             , "z > ve ve~"
                                             , "z > vm vm~" ] )
                ] 

map_wz :: ProcSpecMap
map_wz = 
    HM.fromList [ (Nothing        , MGProc [] [ "p p > w+ z"
                                              , "p p > w- z" ])
                , (Just (3, 24,[]), MGProc [] [ "w+ > l+ vl" ])
                , (Just (3,-24,[]), MGProc [] [ "w- > l- vl~" ]) 
                , (Just (4, 23,[]), MGProc [] [ "z > l+ l- "
                                              , "z > ve ve~"
                                              , "z > vm vm~" ] )
                ] 

map_ww :: ProcSpecMap
map_ww = 
    HM.fromList [ (Nothing        , MGProc [] [ "p p > w+ w-" ])
                , (Just (3, 24,[]), MGProc [] [ "w+ > l+ vl" ])
                , (Just (3,-24,[]), MGProc [] [ "w- > l- vl~" ]) 
                , (Just (4, 24,[]), MGProc [] [ "w+ > l+ vl" ])
                , (Just (4,-24,[]), MGProc [] [ "w- > l- vl~" ]) 
                ] 

procdir :: ProcDir
procdir = ProcDir { pdWorkDirPrefix = "multiproc" 
                  , pdRemoteDirBase = "multiproc"
                  , pdRemoteDirPrefix = "multiproc" }


mproc :: MultiProc
mproc = mkMultiProc procdir [ SingleProc "zz" p_zz map_zz mgrunsetup
                            , SingleProc "wz" p_wz map_wz mgrunsetup
                            , SingleProc "ww" p_ww map_ww mgrunsetup ] 

mgrunsetup :: NumOfEv -> SetNum -> RunSetup
mgrunsetup (NumOfEv nev) (SetNum sn) = 
  RS { numevent = nev
     , machine = LHC8 ATLAS
     , rgrun   = Auto
     , rgscale = 200.0
     , match   = NoMatch
     , cut     = NoCut 
     , pythia  = NoPYTHIA 
     , lhesanitizer = []
     , pgs     = NoPGS
     , uploadhep = NoUploadHEP
     , setnum  = sn
     }

getScriptSetup :: IO ScriptSetup
getScriptSetup = do 
  homedir <- getHomeDirectory
  mdldir <- (</> "template") <$> PModel.getDataDir
  rundir <- (</> "template") <$> PMadGraph.getDataDir 
  return $ 
    SS { modeltmpldir = mdldir 
       , runtmpldir = rundir 
       , sandboxdir = homedir </> "temp/montecarlo/sandbox"
       , mg5base    = homedir </> "temp/montecarlo/MG5_aMC_v2_1_0"
       , mcrundir   = homedir </> "temp/montecarlo/mcrun"
       , pythia8dir = ""
       , pythia8toHEPEVT = "" 
       , hepevt2stdhep = "" 
       }


work :: (Model model) => 
        model 
     -> ScriptSetup 
     -> MultiProc 
     -> ModelParam model 
     -> (String,NumOfEv,SetNum) 
     -> IO () 
work mdl ssetup mp param (pname,nev,sn) = do
  case M.lookup pname (mpMultiProcessParts mp) of 
    Nothing -> return ()
    Just sproc -> do 
      let pdir = mpProcDir mp
      genPhase1 mdl ssetup pdir sproc param (nev,sn)
      genPhase2 mdl ssetup pdir sproc param (nev,sn)

main :: IO () 
main = do 
  updateGlobalLogger "MadGraphAuto" (setLevel DEBUG)
  ssetup <- getScriptSetup
  work SM ssetup mproc SMParam ("zz", NumOfEv 10000,SetNum 1)
  work SM ssetup mproc SMParam ("wz", NumOfEv 10000,SetNum 1)
  work SM ssetup mproc SMParam ("ww", NumOfEv 10000,SetNum 1)


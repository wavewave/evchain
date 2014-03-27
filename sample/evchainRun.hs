{-# LANGUAGE ScopedTypeVariables, NoMonomorphismRestriction, RecordWildCards #-}

module Main where

import           Control.Applicative
import qualified Data.HashMap.Lazy as HM
import           System.Directory
import           System.FilePath ((</>))
import           System.Log.Logger
-- 
import HEP.Automation.EventChain.Driver 
import HEP.Automation.EventChain.Type.MultiProcess
import HEP.Automation.EventChain.Type.Spec
import HEP.Automation.EventChain.Type.Process
import HEP.Automation.EventChain.SpecDSL
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

p_top :: DDecay
p_top = d (top, [t wplus, t bottom])

p_antitop :: DDecay
p_antitop = d (antitop, [t wminus, t antibottom])

p_ttbar :: DCross 
p_ttbar = x (t proton, t proton, [p_top, p_antitop])

map_ttbar :: ProcSpecMap
map_ttbar = 
    HM.fromList [(Nothing             , MGProc [] [ "p p > t t~ QED=0" ])
                ,(Just (3,6,[]) , MGProc []
                                         [ "t > W+ b " ] ) 
                ,(Just (4,-6,[]), MGProc [] 
                                         [ "t~ > W- b~ " ] )
                ] 

proc :: SingleProc
proc = SingleProc "ttbar_decay" p_ttbar map_ttbar mgrunsetup

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

pdir :: ProcDir
pdir = ProcDir { pdWorkDirPrefix = "testttbar" 
               , pdRemoteDirBase = "testttbar"
               , pdRemoteDirPrefix = "testttbar" }

 
main :: IO () 
main = do 
  updateGlobalLogger "MadGraphAuto" (setLevel DEBUG)
  ssetup <- getScriptSetup
  genPhase1 SM ssetup pdir proc SMParam (NumOfEv 10000,SetNum 1)
  genPhase2 SM ssetup pdir proc SMParam (NumOfEv 10000,SetNum 1)


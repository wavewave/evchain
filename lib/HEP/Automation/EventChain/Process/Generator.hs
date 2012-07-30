{-# LANGUAGE ScopedTypeVariables, RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Automation.EventChain.Process.Generator
-- Copyright   : (c) 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- generate events using madgraph-auto . 
-- 
-----------------------------------------------------------------------------

module HEP.Automation.EventChain.Process.Generator where

-- from other packages from others
import           Control.Monad.Reader 
import           Control.Monad.Error
import           Control.Monad.State 
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.Maybe 
import qualified Data.HashMap.Lazy as HM 
import           System.FilePath
import           System.IO
-- from hep-platform packages 
import           HEP.Automation.MadGraph.Model
import           HEP.Automation.MadGraph.Model.SM
import           HEP.Automation.MadGraph.Machine
import           HEP.Automation.MadGraph.SetupType
import           HEP.Automation.MadGraph.UserCut
import           HEP.Automation.MadGraph.Util 
import           HEP.Automation.MadGraph.Run
import           HEP.Storage.WebDAV
import           HEP.Parser.LHEParser.Type
-- from this package 
import           HEP.Automation.EventChain.FileDriver 
import           HEP.Automation.EventChain.Process
import           HEP.Automation.EventChain.Match 
import           HEP.Automation.EventChain.Type.Skeleton
import           HEP.Automation.EventChain.Type.Spec 


------- 
scriptsetup :: ScriptSetup
scriptsetup = SS {
    templatedir = "/Users/wavewave/repo/src/pipeline/template/"
  , workingdir = "/Users/wavewave/repo/workspace/montecarlo/work/"
  , mg5base    = "/Users/wavewave/repo/workspace/montecarlo/MadGraph5_v1_3_33/"
  , workbase   = "/Users/wavewave/repo/workspace/montecarlo/mc/"
  }



processSetup :: String -> ProcessSetup SM
processSetup pname = PS {  
    model = SM
  , process = pname -- "\ngenerate P P > t t~ QED=99\n"
  , processBrief = "TTBar" 
  , workname   = "Test20120727TTBar"
  }

pset :: ModelParam SM
pset = SMParam


ucut :: UserCut 
ucut = UserCut { 
    uc_metcut = 15.0
  , uc_etacutlep = 2.7
  , uc_etcutlep = 18.0 
  , uc_etacutjet = 2.7
  , uc_etcutjet = 15.0 
}

rsetup :: Int -> RunSetup SM 
rsetup n = RS { param = SMParam 
              , numevent = n 
              , machine = TeVatron
              , rgrun   = Fixed
              , rgscale = 200.0
              , match   = NoMatch
              , cut     = NoCut 
              , pythia  = NoPYTHIA
              , usercut = NoUserCutDef 
              , lhesanitizer = NoLHESanitize 
              , pgs     = NoPGS
              , jetalgo = Cone 0.4
              , uploadhep = NoUploadHEP
              , setnum  = 1
              }

wsetup :: String -> Int -> WorkSetup SM 
wsetup str n 
  = WS scriptsetup (processSetup str) (rsetup n) 
       (CS NoParallel) (WebDAVRemoteDir "")




work :: String -> Int -> IO String 
work str n  = do 
    putStrLn "models : sm "
    r <- flip runReaderT (wsetup str n) . runErrorT $ do 
        WS ssetup psetup rsetup _ _ <- ask 
        createWorkDir ssetup psetup
        cardPrepare                      
        generateEvents   
        let taskname = makeRunName psetup rsetup  
        wdir <- getWorkDir 
        let fname = wdir </> "Events" </> taskname ++ "_unweighted_events.lhe.gz"
        return fname 
    case r of 
        Left msg -> error msg 
        Right str -> return str 



testmadgraphX :: CrossID ProcessInfo -> Int -> IO FilePath  
testmadgraphX MkC {..} n = do str <-  work xnode n 
                              putStrLn str 
                              return str 
                              

testmadgraphD :: DecayID ProcessInfo -> Int -> IO FilePath 
testmadgraphD = error "not implemented"

lheCounter :: CrossID ProcessInfo -> FilePath -> IO Counter 
lheCounter cross fp = do 
    h <- openFile fp ReadMode
    evtsHandle True h $$ CL.mapM_ (countingSingleEvent cross.fromJust) --- CL.consume  
    -- print lst 
    return (Counter HM.empty HM.empty)


countingSingleEvent :: CrossID ProcessInfo -> LHEvent -> IO () 
countingSingleEvent cross ev@LHEvent {..} = do 
    r <- matchX cross ev  -- evalStateT (runErrorT m) lhe_ptlinfos
    case r of 
      Left err -> print err
      Right a -> print a
    return () 

  -- where m = matchX cross --  match1 Out 6 


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
import           Control.Concurrent (threadDelay)
import           Control.Monad.Reader 
import           Control.Monad.Error
import           Control.Monad.State 
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.Maybe 
import           Data.Hashable (hash)
import qualified Data.HashMap.Lazy as HM 
import           Numeric
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
import           HEP.Automation.EventChain.Type.Match 
import           HEP.Automation.EventChain.Type.Process
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



processSetup :: String -> String -> ProcessSetup SM
processSetup pname wname = PS {  
    model = SM
  , process = pname 
  , processBrief = "TTBar" 
  , workname   = wname 
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

wsetup :: String -> String -> Int -> WorkSetup SM 
wsetup str wname n 
  = WS scriptsetup (processSetup str wname) (rsetup n) 
       (CS NoParallel) (WebDAVRemoteDir "")




work :: String -> String -> Int -> IO String 
work str wname n  = do 
    putStrLn "models : sm "
    r <- flip runReaderT (wsetup str wname n) . runErrorT $ do 
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


-- | 

lheCntX :: (Show p) => CrossID p -> FilePath -> IO Counter 
lheCntX cross fp = do 
    h <- openFile fp ReadMode
    r <- evtsHandle True h =$= CL.map fromJust 
          $$ CL.foldM (cnt1EvtX cross) (Counter HM.empty HM.empty)   
    return r


-- |

cnt1EvtX :: (Show p) => CrossID p -> Counter -> LHEvent -> IO Counter 
cnt1EvtX cross (Counter incomingm outgoingm) ev@LHEvent {..}  = do 
    r <- matchX cross ev 
    case r of 
      Left err -> fail err
      Right MLHEvent {..} -> do 
        let f (Right (pid,_),pinfo) m = HM.insertWith (+) (pid,idup pinfo) 1 m
            f (Left pid,pinfo) m = HM.insertWith (+) (pid,idup pinfo) 1 m  
            rim = foldr f incomingm mlhev_incoming 
            rom = foldr f outgoingm mlhev_outgoing
        return (Counter rim rom)

-- | 

lheCntD :: (Show p) => PDGID -> DecayID p -> FilePath -> IO Counter
lheCntD i decay fp = do 
    h <- openFile fp ReadMode
    r <- evtsHandle True h =$= CL.map fromJust 
          $$ CL.foldM (cnt1EvtD i decay) (Counter HM.empty HM.empty)   
    return r

-- | 

cnt1EvtD :: (Show p) => PDGID -> DecayID p -> Counter -> LHEvent -> IO Counter 
cnt1EvtD i decay (Counter incomingm outgoingm) ev@LHEvent {..} = do 
    r <- matchD i decay ev 
    case r of 
      Left err -> fail err
      Right MLHEvent {..} -> do 
        let f (Right (pid,_),pinfo) m = HM.insertWith (+) (pid,idup pinfo) 1 m
            f (Left pid,pinfo) m = HM.insertWith (+) (pid,idup pinfo) 1 m  
            rim = foldr f incomingm mlhev_incoming 
            rom = foldr f outgoingm mlhev_outgoing
        return (Counter rim rom)


-- | 

generateX :: ProcSpecMap -> CrossID ProcSmplIdx -> Int -> IO FilePath  
generateX pm MkC {..} n = do 
    case HM.lookup Nothing pm of 
      Nothing -> fail "what? no root process in map?"
      Just str -> do 
        let nwname = "Test"++ show (hash (str,[] :: ProcSmplIdx))  
        print nwname 
        r <- work str nwname n 
        threadDelay 1000000
        return r 

-- | Single PDGID in dnode is assumed. 

generateD :: ProcSpecMap -> DecayID ProcSmplIdx -> Int -> IO FilePath
generateD pm MkD {..} n = do 
    let psidx = (proc_procid . head . ptl_procs) dnode 
        pdgid' = (proc_pdgid . head . ptl_procs ) dnode
        pmidx  = mkPMIdx psidx pdgid' 
    case HM.lookup pmidx pm of 
      Nothing -> fail "cannot find process for pmidx"
      Just str -> do 
        let nwname = "Test"++ show (hash (str,pmidx))  
        print nwname 
        r <- work str nwname n 
        threadDelay 1000000
        return r   

--------
--
---------



testmadgraphX :: CrossID ProcessInfo -> Int -> IO FilePath  
testmadgraphX MkC {..} n = do str <-  work xnode "TestMadGraph" n  
                              putStrLn str 
                              return str 
                              

testmadgraphD :: DecayID ProcessInfo -> Int -> IO FilePath 
testmadgraphD = error "not implemented"


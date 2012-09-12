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
import           Control.Applicative 
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
import           HEP.Automation.MadGraph.Model.ADMXUDD
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
-- 
import qualified Paths_madgraph_auto as PMadGraph 
import qualified Paths_madgraph_auto_model as PModel 

-- |  
getScriptSetup :: FilePath  -- ^ sandboxdir
               -> FilePath  -- ^ mg5base 
               -> FilePath  -- ^ mcrundir 
               -> IO ScriptSetup
getScriptSetup dir_sb dir_mg5 dir_mc = do 
  mdldir <- (</> "template") <$> PModel.getDataDir
  rundir <- (</> "template") <$> PMadGraph.getDataDir 
  return $ 
    SS { modeltmpldir = mdldir 
       , runtmpldir = rundir 
       , sandboxdir = dir_sb -- "/home/wavewave/repo/workspace/montecarlo/working" 
       , mg5base    = dir_mg5 -- "/home/wavewave/repo/ext/MadGraph5_v1_4_8_4/"
       , mcrundir   = dir_mc -- "/home/wavewave/repo/workspace/montecarlo/mc/"
       }

-- | 
processSetup :: String -> String -> ProcessSetup ADMXUDD
processSetup pname wname = PS { model = ADMXUDD
                              , process = pname 
                              , processBrief = "multijet" 
                              , workname = wname 
                              }


-- | 
ucut :: UserCut 
ucut = UserCut { 
    uc_metcut = 15.0
  , uc_etacutlep = 2.7
  , uc_etcutlep = 18.0 
  , uc_etacutjet = 2.7
  , uc_etcutjet = 15.0 
}


-- | 
getRSetup :: ModelParam ADMXUDD -> Int -> RunSetup ADMXUDD 
getRSetup pset n = 
              RS { param = pset
                 , numevent = n
                 , machine = LHC7 ATLAS
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

-- | 
getWSetup :: (FilePath,FilePath,FilePath)
          -> ModelParam ADMXUDD 
          -> String 
          -> String 
          -> Int 
          -> IO (WorkSetup ADMXUDD)
getWSetup (dir_sb,dir_mg5,dir_mc) pset str wname n = 
    WS <$> getScriptSetup dir_sb dir_mg5 dir_mc 
       <*> pure (processSetup str wname)  
       <*> pure (getRSetup pset n) 
       <*> pure (CS NoParallel) 
       <*> pure (WebDAVRemoteDir "")


-- | 
work :: (FilePath,FilePath,FilePath)
     -> ModelParam ADMXUDD -> String -> String -> Int -> IO String 
work (dir_sb,dir_mg5,dir_mc) pset str wname n  = do 
    putStrLn "models : admxudd "
    wsetup <- getWSetup (dir_sb,dir_mg5,dir_mc) pset str wname n  
    r <- flip runReaderT wsetup . runErrorT $ do 
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
generateX :: (FilePath,FilePath,FilePath) 
          -> ModelParam ADMXUDD 
          -> ProcSpecMap 
          -> CrossID ProcSmplIdx 
          -> Int 
          -> IO FilePath  
generateX (dir_sb,dir_mg5,dir_mc) pset pm MkC {..} n = do 
    case HM.lookup Nothing pm of 
      Nothing -> fail "what? no root process in map?"
      Just str -> do 
        let nwname = "Test"++ show (hash (str,[] :: ProcSmplIdx)) 
        print nwname 
        r <- work (dir_sb,dir_mg5,dir_mc) pset str nwname n 
        threadDelay 1000000
        return r 

-- | Single PDGID in dnode is assumed. 
generateD :: (FilePath,FilePath,FilePath)
          -> ModelParam ADMXUDD 
          -> ProcSpecMap 
          -> DecayID ProcSmplIdx 
          -> Int 
          -> IO FilePath
generateD (dir_sb,dir_mg5,dir_mc) pset pm MkD {..} n = do 
    let psidx = (proc_procid . head . ptl_procs) dnode 
        pdgid' = (proc_pdgid . head . ptl_procs ) dnode
        pmidx  = mkPMIdx psidx pdgid' 
    case HM.lookup pmidx pm of 
      Nothing -> fail $ "cannot find process for pmidx = " ++ show pmidx
      Just str -> do 
        let nwname = "Test"++ show (hash (str,pmidx))  
        print nwname 
        r <- work (dir_sb,dir_mg5,dir_mc) pset str nwname n 
        threadDelay 1000000
        return r   

--------
--
---------


{-
testmadgraphX :: CrossID ProcessInfo -> Int -> IO FilePath  
testmadgraphX MkC {..} n = do str <-  work xnode "TestMadGraph" n  
                              putStrLn str 
                              return str 
                              

testmadgraphD :: DecayID ProcessInfo -> Int -> IO FilePath 
testmadgraphD = error "not implemented"

-}
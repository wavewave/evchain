{-# LANGUAGE ScopedTypeVariables, RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Automation.EventChain.Process.Generator
-- Copyright   : (c) 2012,2013 Ian-Woo Kim
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
import           Control.Monad 
import           Control.Monad.Reader 
import           Control.Monad.Error
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.Digest.Pure.MD5
import qualified Data.HashMap.Lazy as HM 
import           System.Directory 
import           System.FilePath
import           System.IO
-- from hep-platform packages 
import           HEP.Automation.MadGraph.Model
import           HEP.Automation.MadGraph.Machine
import           HEP.Automation.MadGraph.SetupType
-- import           HEP.Automation.MadGraph.UserCut
import           HEP.Automation.MadGraph.Util 
import           HEP.Automation.MadGraph.Run
import           HEP.Parser.LHE.Type
import           HEP.Storage.WebDAV
-- from this package 
import           HEP.Automation.EventChain.File 
import           HEP.Automation.EventChain.Match 
import           HEP.Automation.EventChain.Process
import           HEP.Automation.EventChain.Type.Match 
import           HEP.Automation.EventChain.Type.Process
import           HEP.Automation.EventChain.Type.Skeleton
import           HEP.Automation.EventChain.Type.Spec 
-- 
-- import qualified Paths_madgraph_auto as PMadGraph 
-- import qualified Paths_madgraph_auto_model as PModel 

{- 
-- |  
getScriptSetup :: FilePath  -- ^ model directory 
               -> FilePath  -- ^ sandbox directory 
               -> FilePath  -- ^ mg5base 
               -> FilePath  -- ^ main montecarlo run 
               -> IO ScriptSetup
getScriptSetup dir_model dir_sb dir_mg5 dir_mc = do 
  -- mdldir <- (</> "template") <$> PModel.getDataDir
  dir_tmpl <- (</> "template") <$> PMadGraph.getDataDir 
  return $ 
    SS { modeltmpldir = dir_mdl
       , runtmpldir = dir_tmpl 
       , sandboxdir = dir_sb 
       , mg5base    = dir_mg5
       , mcrundir   = dir_mc 
       }
-}


-- | 
processSetupPart :: model  
                 -> String 
                 -> String 
                 -> ProcessSetup model
processSetupPart mdl pname wname = PS { model = mdl
                                      , process = pname 
                                      , processBrief = "part" 
                                      , workname = wname 
                                      }


-- | 
processSetupCombined :: model  
                 -> String 
                 -> String 
                 -> ProcessSetup model
processSetupCombined mdl pname wname = PS { model = mdl
                                          , process = pname  -- this is dummy 
                                          , processBrief = pname
                                          , workname = wname 
                                          }

-- | 
runSetupPart :: ModelParam model -> Int -> RunSetup model 
runSetupPart pset n = 
              RS { param = pset
                 , numevent = n
                 , machine = LHC7 ATLAS
                 , rgrun   = Fixed
                 , rgscale = 200.0
                 , match   = NoMatch
                 , cut     = NoCut 
                 , pythia  = NoPYTHIA
                 , lhesanitizer = NoLHESanitize 
                 , pgs     = NoPGS
                 , jetalgo = Cone 0.4
                 , uploadhep = NoUploadHEP
                 , setnum  = 1
                 }

-- | 
getWorkSetupPart :: model 
              -> ScriptSetup 
              -> ModelParam model  
              -> String 
              -> String 
              -> Int 
              -> WorkSetup model
getWorkSetupPart mdl ssetup pset str wname n = 
    WS ssetup 
       (processSetupPart mdl str wname) 
       (runSetupPart pset n) 
       (WebDAVRemoteDir "")

 
-- | 
getWorkSetupCombined :: model 
                     -> ScriptSetup 
                     -> ModelParam model  
                     -> (String,String) -- ^ (directory name, process name)
                     -> MGRunSetup 
                     -> WorkSetup model
getWorkSetupCombined mdl ssetup pset (wname,str) mgrs = 
  WS ssetup 
     (processSetupCombined mdl str wname)
     (mGRunSetup2RunSetup pset mgrs) 
     (WebDAVRemoteDir "")


-- | 
work :: (Model model) => WorkSetup model -> IO String 
work wsetup   = do 
    r <- flip runReaderT wsetup . runErrorT $ do 
      WS ssetup psetup rsetup _ <- ask 
      let wb = mcrundir ssetup
          wn = workname psetup 
      b <- liftIO $ doesDirectoryExist (wb </> wn)
      when (not b) $ createWorkDir ssetup psetup
      cardPrepare                      
      generateEvents   
      let taskname = makeRunName psetup rsetup  
      wdir <- getWorkDir 
      let fname = wdir </> "Events" </> taskname </> taskname ++ "_unweighted_events.lhe.gz"
      return fname 
    case r of 
      Left msg -> error msg 
      Right str -> return str 


-- | 
lheCntX :: (Show p) => CrossID p -> FilePath -> IO Counter 
lheCntX cross fp = do 
    h <- openFile fp ReadMode
    evtsHandle True h $$ CL.foldM (cnt1EvtX cross) (Counter HM.empty HM.empty)   



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
    evtsHandle True h $$ 
      CL.foldM (cnt1EvtD i decay) (Counter HM.empty HM.empty)   

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
generateX :: (Model model) => 
             model 
          -> ScriptSetup 
          -> (String,String)              -- ^ (base madgraph dir name, resultant process name) 
          -> ModelParam model             -- ^ model parameters 
          -> ProcSpecMap                  -- ^ 
          -> CrossID ProcSmplIdx 
          -> Int 
          -> IO FilePath  
generateX mdl ssetup (basename,procname) pset pm MkC {..} n = do 
    case HM.lookup Nothing pm of 
      Nothing -> fail "what? no root process in map?"
      Just str -> do 
        let nwname = basename
        print nwname 
        work (getWorkSetupPart mdl ssetup pset str nwname n)


-- | Single PDGID in dnode is assumed. 
--   (why non-exhautive pattern here?) 
generateD :: (Model model) => 
             model 
          -> ScriptSetup 
          -> (String,String)              -- ^ (base madgraph dir name, resultant process name) 
          -> ModelParam model             -- ^ model parameter 
          -> ProcSpecMap 
          -> DecayID ProcSmplIdx 
          -> Int 
          -> IO FilePath
generateD mdl ssetup (basename,procname) pset pm MkD {..} n = do 
    let psidx = (proc_procid . head . ptl_procs) dnode 
        pdgid' = (proc_pdgid . head . ptl_procs ) dnode
        pmidx  = mkPMIdx psidx pdgid' 
    case HM.lookup pmidx pm of 
      Nothing -> fail $ "cannot find process for pmidx = " ++ show pmidx
      Just str -> do 
        let nwname = (((basename++"_") ++).show.md5.B.pack.(str ++).show) pmidx
        print nwname 
        work (getWorkSetupPart mdl ssetup pset str nwname n)


combineX :: (Model model) => 
             model 
          -> ScriptSetup 
          -> (String,String)              -- ^ (base madgraph dir name, resultant process name) 
          -> ModelParam model             -- ^ model parameters 
          -> MGRunSetup
          -> IO (FilePath,FilePath,WorkSetup model)
combineX mdl ssetup (basename,procname) pset mgrs = do 
    let nwname = basename
    print nwname 
    let wsetup =  getWorkSetupCombined 
                    mdl ssetup pset (basename,procname) mgrs 
    r <- flip runReaderT wsetup . runErrorT $ do 
      WS _ psetup rsetup _ <- ask 
      let taskname = makeRunName psetup rsetup
      wdir <- getWorkDir 
      let dir = wdir </> "Events" </> taskname 
          file = taskname ++ "_unweighted_events.lhe.gz"
      return (dir,file,wsetup) 
    case r of 
      Left msg -> error msg 
      Right str -> return str 



{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Automation.EventChain.Process.Generator
-- Copyright   : (c) 2012,2013,2015 Ian-Woo Kim
--
-- License     : GPL-3
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
import           Data.Digest.Pure.MD5 (md5)
import           Data.Hashable (hash)
import qualified Data.HashMap.Lazy as HM 
import           System.Directory 
import           System.FilePath
import           System.IO
-- from hep-platform packages 
import           HEP.Automation.MadGraph.Model
import           HEP.Automation.MadGraph.SetupType
import           HEP.Automation.MadGraph.Type 
import           HEP.Automation.MadGraph.Util 
import           HEP.Automation.MadGraph.Run
import           HEP.Parser.LHE.Type
import           HEP.Parser.LHE.Sanitizer.Type
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


-- | 
processSetupPart :: model  
                 -> ProcessInfo
                 -> String 
                 -> HashSalt
                 -> ProcessSetup model
processSetupPart mdl ps wname hs = PS { model = mdl
                                   , process = ps 
                                   , processBrief = "part" 
                                   , workname = wname 
                                   , hashSalt = hs  
                                   }


-- | 
processSetupCombined :: model  
                 -> String 
                 -> String 
                 -> ProcessSetup model
processSetupCombined mdl pname wname = 
  PS { model = mdl
     , process = MGProc [] []    -- dummy process
     , processBrief = pname
     , workname = wname 
     , hashSalt = HashSalt Nothing 
     }

-- | 
runSetupPart :: RunSetup -> Int -> RunSetup  
runSetupPart tmpl n =  tmpl { numevent = n, match = NoMatch, cut = NoCut, pythia = NoPYTHIA 
                            , lhesanitizer = [Shuffle, Blobize], pgs = NoPGS, uploadhep = NoUploadHEP } 
{-    RS { numevent = n
       , machine = LHC7 ATLAS
       , rgrun   = Fixed
       , rgscale = 200.0
       , match   = NoMatch
       , cut     = NoCut 
       , pythia  = NoPYTHIA
       , lhesanitizer = [Shuffle, Blobize]
       , pgs     = NoPGS
       , uploadhep = NoUploadHEP
       , setnum  = 1
       } -}

-- | 
getWorkSetupPart :: model 
              -> ScriptSetup 
              -> RunSetup
              -> ModelParam model  
              -> ProcessInfo 
              -> String 
              -> HashSalt
              -> Int 
              -> WorkSetup model
getWorkSetupPart mdl ssetup rset pset str wname hs n = 
    WS ssetup 
       (processSetupPart mdl str wname hs) 
       pset 
       (runSetupPart rset n)
       (WebDAVRemoteDir "")

 
-- | 
getWorkSetupCombined :: model 
                     -> ScriptSetup 
                     -> RunSetup 
                     -> ModelParam model  
                     -> (String,String) -- ^ (directory name, process name)
                     -> WorkSetup model
getWorkSetupCombined mdl ssetup rset pset (wname,str) = 
  WS ssetup 
     (processSetupCombined mdl str wname)
     pset 
     rset
     (WebDAVRemoteDir "")


-- | 
work :: (Model model) => WorkSetup model -> IO String 
work wsetup   = do 
    r <- flip runReaderT wsetup . runErrorT $ do 
      ws <- ask 
      let (ssetup,psetup,param,rsetup) = 
             ((,,,) <$> ws_ssetup <*> ws_psetup <*> ws_param <*> ws_rsetup) ws 
      let wb = mcrundir ssetup
          wn = workname psetup 
      b <- liftIO $ doesDirectoryExist (wb </> wn)
      when (not b) $ createWorkDir ssetup psetup
      cardPrepare                      
      generateEvents
      -- sanitize if LHESanitize is on
      case lhesanitizer rsetup of
        [] -> return ()
        _:_ -> sanitizeLHE
      --    
      let taskname = makeRunName psetup param rsetup  
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
cnt1EvtX cross (Counter incomingm outgoingm) ev = do 
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
cnt1EvtD i decay (Counter incomingm outgoingm) ev = do 
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
          -> RunSetup 
          -> (String,String)              -- ^ (base madgraph dir name, resultant process name) 
          -> ModelParam model             -- ^ model parameters 
          -> ProcSpecMap                  -- ^ 
          -> CrossID ProcSmplIdx 
          -> Int 
          -> IO FilePath  
generateX mdl ssetup rset (basename,procname) pset pm MkC {..} n = do 
    case HM.lookup Nothing pm of 
      Nothing -> fail "what? no root process in map?"
      Just strs -> do 
        let nwname = basename
        work (getWorkSetupPart mdl ssetup rset pset strs nwname (HashSalt Nothing) n)


-- | Single PDGID in dnode is assumed. 
--   (why non-exhautive pattern here?) 
generateD :: (Model model) => 
             model 
          -> ScriptSetup 
          -> RunSetup 
          -> (String,String)              -- ^ (base madgraph dir name, resultant process name) 
          -> ModelParam model             -- ^ model parameter 
          -> ProcSpecMap 
          -> ProcessIndex 
          -> DecayID ProcSmplIdx 
          -> Int 
          -> IO FilePath
generateD mdl ssetup rset (basename,procname) pset pm procidx MkD {..} n = do 
    let psidx = (proc_procid . head . ptl_procs) dnode 
        pdgid' = (proc_pdgid . head . ptl_procs ) dnode
        pmidx  = mkPMIdx psidx pdgid' 
    case HM.lookup pmidx pm of 
      Nothing -> fail $ "cannot find process for pmidx = " ++ show pmidx
      Just proc@(MGProc _ strs) -> do 
        let nwname = (((basename++"_") ++).show.md5.B.pack.(concat strs ++)) (show procidx ++ show pmidx)
            hs = HashSalt (Just (hash nwname `mod` 1000000 + 1000001))
        putStrLn ("procidx = " ++ show procidx) 
        putStrLn ("dnone = " ++ show dnode)
        putStrLn ("pmidx = " ++ show pmidx)
        putStrLn ("nwname = " ++ nwname) 
        -- print hs 
        work (getWorkSetupPart mdl ssetup rset pset proc nwname hs n)


combineX :: (Model model) => 
             model 
          -> ScriptSetup 
          -> RunSetup
          -> (String,String)              -- ^ (base madgraph dir name, resultant process name) 
          -> ModelParam model             -- ^ model parameters 
          -> IO (FilePath,FilePath,WorkSetup model)
combineX mdl ssetup rset (basename,procname) pset = do 
    let nwname = basename
    print nwname 
    let wsetup =  getWorkSetupCombined 
                    mdl ssetup rset pset (basename,procname) 
    r <- flip runReaderT wsetup . runErrorT $ do 
      ws <- ask 
      let (ssetup,psetup,param,rsetup) = 
             ((,,,) <$> ws_ssetup <*> ws_psetup <*> ws_param <*> ws_rsetup) ws 
      let taskname = makeRunName psetup param rsetup
      wdir <- getWorkDir 
      let dir = wdir </> "Events" </> taskname 
          file = taskname ++ "_unweighted_events.lhe.gz"
      return (dir,file,ws) 
    case r of 
      Left msg -> error msg 
      Right str -> return str 



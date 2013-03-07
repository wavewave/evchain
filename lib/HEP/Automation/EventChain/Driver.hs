-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Automation.EventChain.Driver
-- Copyright   : (c) 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- Main evchain process
-- 
-----------------------------------------------------------------------------

module HEP.Automation.EventChain.Driver where

import qualified Codec.Compression.GZip as GZ
import           Control.Applicative
import           Control.Monad 
import           Control.Monad.Error 
import           Control.Monad.State 
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as LC8 
import           Data.Conduit
import           Data.Conduit.Binary 
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Util.Control as CU 
import           Data.Conduit.Zlib
import qualified Data.Traversable as T
import qualified Data.HashMap.Lazy as HM
import           Data.Maybe 
import           System.Directory 
import           System.Posix.Env 
import           System.FilePath 
import           System.IO
import           System.Process
import           Text.XML.Stream.Parse
import           Text.XML.Stream.Render
-- from hep-platform 
import           HEP.Automation.MadGraph.Model
import           HEP.Automation.MadGraph.SetupType
import           HEP.Parser.LHEParser.Parser.Conduit
import           HEP.Parser.LHEParser.Type
import           HEP.Storage.WebDAV
-- from this package 
import           HEP.Automation.EventChain.File
import           HEP.Automation.EventChain.LHEConn
import           HEP.Automation.EventChain.Print 
import           HEP.Automation.EventChain.Process
import           HEP.Automation.EventChain.Process.Generator
import           HEP.Automation.EventChain.Simulator 
import           HEP.Automation.EventChain.SpecDSL
import           HEP.Automation.EventChain.Type.Skeleton
import           HEP.Automation.EventChain.Type.Spec
import           HEP.Automation.EventChain.Type.Process
--
import Debug.Trace

{-
dummyEvInfo :: EventInfo 
dummyEvInfo = EvInfo 0 0 0 0 0 0
-}


getheader :: FilePath -> IO B.ByteString 
getheader fp = do 
  temph <- openFile fp ReadMode  
  v <- sourceHandle temph =$= ungzip =$= parseBytes def $$ CL.consume
  hClose temph 
  let r = Prelude.takeWhile (not.isEventStart) v
  bstrs <- CL.sourceList r =$= renderBytes def $$ CL.consume 
  return (B.concat bstrs)

-- | main driver for evchain program
evchainGen :: (Model model) => 
              model 
           -> ScriptSetup
           -> (String,String)
           -> ModelParam model 
           -> ProcSpecMap 
           -> DCross 
           -> MGRunSetup 
           -> IO () 
evchainGen mdl path (basename,procname) pset pmap cross mgrs = do 
  let idxcross = (mkCrossIDIdx . mkDICross) cross 
  print idxcross
  rm <- createProcessX 
          (generateX mdl path (basename,procname) pset pmap) 
          (generateD mdl path (basename,procname) pset pmap) 
          lheCntX lheCntD idxcross (mgrs_numevent mgrs) 
  let fp = fromJust (HM.lookup [] rm) 
      (_,fn) = splitFileName fp
      (fb,_) = splitExtension fn 
  putStrLn ("start generate in  " ++ fn)
  bstr <- getheader fp 
  rm2 <- makeLHEProcessMap rm 
  let action acc () = do 
        lhev <- accumTotalEvent <$> matchFullCross idxcross 
        let output = lheFormatOutput lhev ++ endl 
        return (acc . (output++))
  let lst = replicate (mgrs_numevent mgrs)  ()
  let r = runState (runErrorT (foldM action id lst)) rm2
  case fst r of 
    Left err -> putStrLn err
    Right builder -> do 
      let builder' = ((C8.unpack bstr)++) . builder
      (dir,file,wsetup) <-
        combineX mdl path (basename,procname) pset mgrs
      b <- doesDirectoryExist dir
      when (not b) (createDirectory dir)
      (LC8.writeFile (dir</>file) . GZ.compress . LC8.pack . builder') []
      putStrLn $ "The resultant file " ++ (dir</>file) ++ " is generated."


-- |
webdavconfig :: String -> WebDAVConfig 
webdavconfig urlbase = WebDAVConfig { webdav_path_wget = "/usr/bin/wget" 
                                    , webdav_path_cadaver = "/usr/bin/cadaver" 
                                    , webdav_baseurl = urlbase }


{- 
-- | 
dummyGen :: (Model model) => 
              model 
           -> (FilePath,FilePath,FilePath)
           -> ModelParam model 
           -> FilePath 
           -> String 
           -> FilePath 
           -> ProcSpecMap 
           -> DCross 
           -> Int 
           -> IO () 
dummyGen mdl path pset tempdirbase urlbase remotedir pmap cross n = do 
  let idxcross = (mkCrossIDIdx . mkDICross) cross 
  print idxcross
  rm <- createProcessX 
          (dummyX mdl path pset pmap) 
          (dummyD mdl path pset pmap) 
          lheCntX lheCntD idxcross n
  print rm 

  let fp = fromJust (HM.lookup [] rm) 
      (_,fn) = splitFileName fp
      (fb,_) = splitExtension fn 
  print fn 
  bstr <- getheader fp 

  rm2 <- makeLHEProcessMap rm 
  let action acc () = do 
        lhev <- accumTotalEvent <$> matchFullCross idxcross 
        let output = lheFormatOutput lhev ++ endl 
        return (acc . (output++))
  let lst = replicate n ()
  let r = runState (runErrorT (foldM action id lst)) rm2
  case fst r of 
    Left err -> putStrLn err
    Right builder -> do  let builder' = ((C8.unpack bstr)++) . builder
                         -- -- createDirectory tempdirbase 
                         setCurrentDirectory tempdirbase 
                         print fb 
                         writeFile fb (builder' "\n</LesHouchesEvents>\n\n") 
                         let davcfg = webdavconfig urlbase 
                             rdir = WebDAVRemoteDir remotedir 
                         uploadFile davcfg rdir fb 
                         pythia8_run davcfg rdir fb testpythiadir testmcdir  
                         return ()
  return ()
-}

{-

testpythiadir = "/home/wavewave/repo/ext/pythia8165/examples"

testmcdir = "/home/wavewave/repo/ext/MadGraph5_v1_4_8_4/pptt"


pythia8_run :: WebDAVConfig -> WebDAVRemoteDir 
           -> FilePath 
           -> FilePath 
           -> FilePath 
           -> IO () 
pythia8_run davcfg rdir fp pythiadir mcdir = do 
  -- copyFile 
  --   (mcdir </> "Cards" </> "pythia_card_default.dat") 
  --   (mcdir </> "Cards" </> "pythia_card.dat")
  setCurrentDirectory pythiadir
  fetchFile davcfg rdir fp
  let (fn,_) = splitExtension fp
  -- renameFile fp "unweighted_events.lhe"
  -- putEnv $ "PDG_MASS_TBL=" ++ pythiadir </> "mass_width_2004.mc"
  system (pythiadir </> "main25.exe " ++ fp) 
  
  return ()


-}

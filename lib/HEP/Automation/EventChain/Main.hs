-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Automation.EventChain.Main
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

module HEP.Automation.EventChain.Main where

import           Control.Applicative
import           Control.Monad 
import           Control.Monad.Error 
import           Control.Monad.State 
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import           Data.Conduit
import           Data.Conduit.Binary 
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Util.Control as CU 
import           Data.Conduit.Zlib
import qualified Data.Traversable as T
import qualified Data.HashMap.Lazy as HM
import           Data.Maybe 
import           System.Directory 
import           System.FilePath 
import           System.IO
import           Text.XML.Stream.Parse
import           Text.XML.Stream.Render
-- 
import           HEP.Parser.LHEParser.Parser.Conduit
import           HEP.Parser.LHEParser.Type
import           HEP.Storage.WebDAV
import           HEP.Automation.MadGraph.Model
import           HEP.Automation.MadGraph.Model.ADMXUDD
-- 
import           HEP.Automation.EventChain.LHEConn
import           HEP.Automation.EventChain.FileDriver
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
  -- =$= CU.dropWhile (not.isEventStart) $$ CL.consume
  hClose temph 
  let r = Prelude.takeWhile (not.isEventStart) v
  -- mapM_ (putStrLn.show) r 
  bstrs <- CL.sourceList r =$= renderBytes def $$ CL.consume 
  return (B.concat bstrs)
  -- (Prelude.dropWhile (not.isEventStart) v)


-- | 
evchainGen :: (Model model) => 
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
evchainGen mdl path pset tempdirbase urlbase remotedir pmap cross n = do 
  let idxcross = (mkCrossIDIdx . mkDICross) cross 
  print idxcross
  rm <- createProcessX 
          (generateX mdl path pset pmap) 
          (generateD mdl path pset pmap) 
          lheCntX lheCntD idxcross n
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
                         createDirectory tempdirbase 
                         setCurrentDirectory tempdirbase 
                         print fb 
                         writeFile fb (builder' []) 
                         uploadFile (webdavconfig urlbase) 
                           (WebDAVRemoteDir remotedir) fb 
                         return ()
  return ()

-- |
webdavconfig :: String -> WebDAVConfig 
webdavconfig urlbase = WebDAVConfig { webdav_path_wget = "/usr/bin/wget" 
                                    , webdav_path_cadaver = "/usr/bin/cadaver" 
                                    , webdav_baseurl = urlbase }


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
                         createDirectory tempdirbase 
                         setCurrentDirectory tempdirbase 
                         print fb 
                         writeFile fb (builder' "\n</LesHouchesEvents>\n\n") 
                         uploadFile (webdavconfig urlbase) 
                           (WebDAVRemoteDir remotedir) fb 
                         return ()
  return ()






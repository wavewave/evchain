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
import           Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Traversable as T
import qualified Data.HashMap.Lazy as HM
import           Data.Maybe 
import           System.Directory 
import           System.FilePath 
import           System.IO
-- 
import           HEP.Parser.LHEParser.Type
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

dummyEvInfo :: EventInfo 
dummyEvInfo = EvInfo 0 0 0 0 0 0

-- | 
evchainGen :: ProcSpecMap -> DCross -> Int -> IO () 
evchainGen pmap cross n = do 
  let idxcross = (mkCrossIDIdx . mkDICross) cross 
  print idxcross
  rm <- createProcessX (generateX pmap) (generateD pmap) 
          lheCntX lheCntD idxcross n
  let fp = fromJust (HM.lookup [] rm) 
      (_,fn) = splitFileName fp
      (fb,_) = splitExtension fn 
  print fn 
  rm2 <- makeLHEProcessMap rm 
  let action acc () = do 
        pinfos <- accumTotalEvent <$> matchFullCross idxcross 
        let output = lheFormatOutput (LHEvent dummyEvInfo pinfos) ++ endl 
        return (acc . (output++))
  let lst = replicate n ()
  let r = runState (runErrorT (foldM action id lst)) rm2
  case fst r of 
    Left err -> putStrLn err
    Right builder -> do  putStrLn (builder [])
                         setCurrentDirectory "/home/wavewave"  
                         print fb 
                         writeFile fb (builder [])
  return ()



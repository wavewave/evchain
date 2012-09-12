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
import           System.IO
-- 
import           HEP.Parser.LHEParser.Type
-- 
import           HEP.Automation.EventChain.LHEConn
import           HEP.Automation.EventChain.FileDriver
import           HEP.Automation.EventChain.Type.Skeleton
import           HEP.Automation.EventChain.Type.Spec
import           HEP.Automation.EventChain.Type.Process
import           HEP.Automation.EventChain.SpecDSL
import           HEP.Automation.EventChain.Simulator 
import           HEP.Automation.EventChain.Process
import           HEP.Automation.EventChain.Process.Generator

-- | 
evchainGen :: ProcSpecMap -> DCross -> Int -> IO () 
evchainGen pmap cross n = do 
  let idxcross = (mkCrossIDIdx . mkDICross) cross 
  print idxcross
  rm <- createProcessX (generateX pmap) (generateD pmap) 
          lheCntX lheCntD idxcross n
  rm2 <- makeLHEProcessMap rm 
  let test = replicate n (accumTotalEvent <$> matchFullCross idxcross)
  let r = runState (runErrorT (sequence test)) rm2
  print (fst r)



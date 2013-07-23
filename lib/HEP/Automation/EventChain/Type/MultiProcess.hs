-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Automation.EventChain.Type.MultiProcess
-- Copyright   : (c) 2013 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- Types for MultiProcess Spec 
--
-----------------------------------------------------------------------------

module HEP.Automation.EventChain.Type.MultiProcess 
( NumOfEv (..)
, SetNum (..)
, SingleProc(..)
, ProcDir(..)
, MultiProc 
, mpProcDir
, mpMultiProcessParts
, mkMultiProc
) where

import Control.Applicative ((<$>),(<*>)) 
import qualified Data.Map as M
-- 
import HEP.Automation.MadGraph.SetupType 
--
import HEP.Automation.EventChain.Type.Spec
import HEP.Automation.EventChain.Type.Process

newtype NumOfEv = NumOfEv { unNumOfEv :: Int } deriving (Show)

newtype SetNum = SetNum { unSetNum :: Int } deriving (Show)

data SingleProc = SingleProc { spName :: String 
                             , spCross :: DCross 
                             , spProcSpecMap :: ProcSpecMap
                             , spRunSetup :: NumOfEv -> SetNum -> RunSetup  
                             } 

data ProcDir = ProcDir { pdWorkDirPrefix :: String 
                       , pdRemoteDirBase :: FilePath 
                       , pdRemoteDirPrefix :: String 
                       }

data MultiProc = MultiProc { mpProcDir :: ProcDir 
                           , mpMultiProcessParts :: M.Map String SingleProc 
                           }

mkMultiProc :: ProcDir -> [SingleProc] -> MultiProc 
mkMultiProc pdir sprocs = let lst = map ((,) <$> spName <*> id) sprocs
                          in MultiProc pdir (M.fromList lst) 


{-
mkSingleProc :: String -> DCross -> ProcSpecMap -> (NumOfEv -> SetNum -> RunSetup) -> SingleProc
mkSingleProc n c m r = MultiProcPart n c (mkCrossIDIdx (mkDICross c)) m r 
-}
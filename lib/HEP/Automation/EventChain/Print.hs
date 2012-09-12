-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Automation.EventChain.Print
-- Copyright   : (c) 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- Print format utility 
-- 
-----------------------------------------------------------------------------

module HEP.Automation.EventChain.Print where

-- others
import Data.List (intercalate)
import Text.Printf
-- from hep-platform 
import HEP.Parser.LHEParser.Type
-- this package
-- import HEP.Automation.MadGraph.EventChain.Type

-- | 
endl :: String 
endl = "\n"

-- |
lheFormatOutput :: LHEvent -> String 
lheFormatOutput (LHEvent einfo pinfos) =
  "<event>" ++ endl 
  ++ printf "%2d" (nup einfo) -- ++ "  " 
  ++ printf "%4d" (idprup einfo) ++ " " 
  ++ printf "%14.7E" (xwgtup einfo) ++ " " 
  ++ printf "%14.7E" (scalup einfo) ++ " " 
  ++ printf "%14.7E" (aqedup einfo) ++ " " 
  ++ printf "%14.7E" (aqcdup einfo) ++ endl 
  ++ concatMap pformat pinfos 
  ++ "</event>" -- ++ endl

-- | 
pformat :: PtlInfo -> String 
pformat pinfo = 
    {- printf "{- %3d -}" (ptlid pinfo) ++ -} 
    printf "%9d" (idup  pinfo)
    ++ printf "%5d" (istup pinfo)
    ++ printf "%5d" (fst (mothup pinfo))
    ++ printf "%5d" (snd (mothup pinfo))
    ++ printf "%5d" (fst (icolup pinfo))
    ++ printf "%5d" (snd (icolup pinfo))
    ++ printf "%19.11E" pupx
    ++ printf "%19.11E" pupy
    ++ printf "%19.11E" pupz
    ++ printf "%19.11E" pupt 
    ++ printf "%19.11E" pupm 
    ++ printf "%4.1f" (vtimup pinfo)
    ++ printf "%5.1f" (spinup pinfo)
    ++ endl 
  where (pupx,pupy,pupz,pupt,pupm) = pup pinfo 

-- | 
pformats :: [PtlInfo] -> String
pformats = concatMap pformat 


{-
-- | 
printCrossStr :: CrossID -> String 
printCrossStr (GCross xprocg inc out) = 
    "main process = " ++ show pid ++ endl
     ++ intercalate endl (map printDecay inc) ++ endl
     ++ intercalate endl (map printDecay out) 
--      ++ "length inc = " ++ show (length inc) ++ endl
--      ++ "length out = " ++ show (length out)
  where XNode pid = xprocg  
-}

{-
-- |

printDecay :: DecayID -> String
printDecay (GTerminal (TNode tid)) = "terminal = " ++ show tid
printDecay (GDecay (DNode pdgID procid, gdecays)) = 
    "decay = ( " ++ show pdgID ++ ", " ++ show procid  ++ ", ["
    ++ intercalate "," (map printDecay gdecays)
    ++ "]"
    ++ ")"
-}

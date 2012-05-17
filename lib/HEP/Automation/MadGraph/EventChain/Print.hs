module HEP.Automation.MadGraph.EventChain.Print where

import Text.Printf
import HEP.Parser.LHEParser.Type
import HEP.Automation.MadGraph.EventChain.Type
import Data.List (intercalate)

-- | 

endl :: String 
endl = "\n"

-- | 

printCrossStr :: CrossID -> String 
printCrossStr (GCross inc out xprocg) = 
    "main process = " ++ show pid ++ endl
     ++ intercalate endl (map printDecay inc) ++ endl
     ++ intercalate endl (map printDecay out) 
--      ++ "length inc = " ++ show (length inc) ++ endl
--      ++ "length out = " ++ show (length out)
  where XNode pid = xprocg  

-- |

printDecay :: DecayID -> String
printDecay (GTerminal (TNode tid)) = "terminal = " ++ show tid
printDecay (GDecay (DNode pdgID procid, gdecays)) = 
    "decay = ( " ++ show pdgID ++ ", " ++ show procid  ++ ", ["
    ++ intercalate "," (map printDecay gdecays)
    ++ "]"
    ++ ")"

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


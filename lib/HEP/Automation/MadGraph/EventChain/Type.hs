module HEP.Automation.MadGraph.EventChain.Type where

import HEP.Parser.LHEParser.Type

import qualified Data.IntMap as M
-- import qualified Data.Map as M
import Data.List


data GDecayTop dnode tnode a = GDecay (dnode a,[GDecayTop dnode tnode a])
                             | GTerminal (tnode a)

data GCross xnode dnode tnode a = 
    GCross { incomingG :: [GDecayTop dnode tnode a] 
           , outgoingG :: [GDecayTop dnode tnode a] 
           , xprocG :: xnode } 

-- | 

type ProcessID = Int

-- | 

type ParticleID = Int

-- | 

type EventMap = M.IntMap LHEvent 

data ProcessInfo = ProcessInfo { name :: String }

-- | Terminal Node 

data TNode a = TNode a 

-- | Decay Node

data DNode a = DNode a ProcessID


-- | Cross Node

data XNode = XNode ProcessID

-- | 

type DecayID = GDecayTop DNode TNode (ParticleID,PDGID)

-- | 

type CrossID = GCross XNode DNode TNode (ParticleID,PDGID)

-- | 

data MatchedLHEvent = MLHEvent { mlhev_einfo :: EventInfo
                               , mlhev_incoming :: [(ParticleID,PtlInfo)]
                               , mlhev_outgoing :: [(ParticleID,PtlInfo)]
                               , mlhev_intermediate :: [PtlInfo] } 


-- |

getContent :: GDecayTop DNode TNode a -> a
getContent (GTerminal (TNode x)) = x
getContent (GDecay (DNode x _,_)) = x

-- | 

getProcessIDFromDecayTop :: GDecayTop DNode TNode a -> [ProcessID]
getProcessIDFromDecayTop (GTerminal _) = []
getProcessIDFromDecayTop (GDecay (DNode _ p1,ds)) = p1 : concatMap getProcessIDFromDecayTop ds 
 
-- | 

getProcessIDFromCross :: GCross XNode DNode TNode a -> [ProcessID]
getProcessIDFromCross (GCross inc out (XNode pid)) = 
    pid : concatMap getProcessIDFromDecayTop inc 
    ++ concatMap getProcessIDFromDecayTop out 



endl :: String 
endl = "\n"


printCrossStr :: GCross XNode DNode TNode (ParticleID,PDGID) -> String 
printCrossStr (GCross inc out xprocg) = 
    "main process = " ++ show pid ++ endl
     ++ intercalate endl (map printDecay inc) ++ endl
     ++ intercalate endl (map printDecay out) 
--      ++ "length inc = " ++ show (length inc) ++ endl
--      ++ "length out = " ++ show (length out)

  where XNode pid = xprocg  


printDecay :: GDecayTop DNode TNode (ParticleID,PDGID) -> String
printDecay (GTerminal (TNode tid)) = "terminal = " ++ show tid
printDecay (GDecay (DNode pdgid procid, gdecays)) = 
    "decay = ( " ++ show pdgid ++ ", " ++ show procid  ++ ", ["
    ++ intercalate "," (map printDecay gdecays)
    ++ "]"
    ++ ")"




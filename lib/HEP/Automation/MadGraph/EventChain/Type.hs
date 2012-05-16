module HEP.Automation.MadGraph.EventChain.Type where

import HEP.Parser.LHEParser.Type

import qualified Data.IntMap as M
-- import qualified Data.Map as M
import Data.List

-- | Terminal Node 

data TNode a = TNode a 

-- | Decay Node

data DNode a b = DNode a b -- ProcessID


-- | Cross Node

data XNode b = XNode b -- ProcessID


data GDecayTop dnode tnode a b = GDecay (dnode a b, [GDecayTop dnode tnode a b])
                               | GTerminal (tnode a)

data GCross xnode dnode tnode a b = 
    GCross { incomingG :: [GDecayTop dnode tnode a b] 
           , outgoingG :: [GDecayTop dnode tnode a b]  
           , xprocG :: xnode b } 

-- | 

type ProcessID = Int

-- | 

type ParticleID = Int

-- | 

type EventMap = M.IntMap LHEvent 


-- | 

type DecayID = GDecayTop DNode TNode (ParticleID,PDGID) ProcessID

-- | 

type CrossID = GCross XNode DNode TNode (ParticleID,PDGID) ProcessID

-- | 

data MatchedLHEvent = MLHEvent { mlhev_einfo :: EventInfo
                               , mlhev_incoming :: [(ParticleID,PtlInfo)]
                               , mlhev_outgoing :: [(ParticleID,PtlInfo)]
                               , mlhev_intermediate :: [PtlInfo] } 


-- |

getContent :: GDecayTop DNode TNode a b -> a
getContent (GTerminal (TNode x)) = x
getContent (GDecay (DNode x _,_)) = x

-- | 

getProcessFromDecayTop :: GDecayTop DNode TNode a b -> [b]
getProcessFromDecayTop (GTerminal _) = []
getProcessFromDecayTop (GDecay (DNode _ p1,ds)) = p1 : concatMap getProcessFromDecayTop ds 
 
-- | 

getProcessFromCross :: GCross XNode DNode TNode a b -> [b]
getProcessFromCross (GCross inc out (XNode p)) = 
    p : concatMap getProcessFromDecayTop inc 
    ++ concatMap getProcessFromDecayTop out 



endl :: String 
endl = "\n"


printCrossStr :: CrossID -> String 
printCrossStr (GCross inc out xprocg) = 
    "main process = " ++ show pid ++ endl
     ++ intercalate endl (map printDecay inc) ++ endl
     ++ intercalate endl (map printDecay out) 
--      ++ "length inc = " ++ show (length inc) ++ endl
--      ++ "length out = " ++ show (length out)

  where XNode pid = xprocg  


printDecay :: DecayID -> String
printDecay (GTerminal (TNode tid)) = "terminal = " ++ show tid
printDecay (GDecay (DNode pdgid procid, gdecays)) = 
    "decay = ( " ++ show pdgid ++ ", " ++ show procid  ++ ", ["
    ++ intercalate "," (map printDecay gdecays)
    ++ "]"
    ++ ")"




{-# LANGUAGE FlexibleInstances, FlexibleContexts, StandaloneDeriving #-}

module HEP.Automation.MadGraph.EventChain.Type where

import HEP.Parser.LHEParser.Type

import qualified Data.IntMap as M
-- import qualified Data.Map as M
import Data.List (intercalate)
import Data.Copointed 
import Data.Foldable
import Data.Traversable
import Control.Applicative

import Prelude hiding (concatMap,foldr)

-- | Particle Kind 

data PKind = KPDGID PDGID | MultiJet | PtlPtlbar PDGID 
             deriving (Show,Eq)

-- | 

instance Num PKind where
  _ + _ = error " no + defined for PKind " 
  _ - _ = error " no - defined for PKind "
  _ * _ = error " no - defined for PKind "
  abs _ = error " no abs defined for PKind " 
  signum _ = error " no signum defined for PKind " 
  fromInteger n = KPDGID (fromInteger n)

-- | Terminal Node 

data TNode a = TNode a 

deriving instance (Show a) => Show (TNode a)

deriving instance (Eq a) => Eq (TNode a)

-- deriving instance (Num a) => Num (TNode a)

-- | Decay Node

data DNode a b = DNode a b -- ProcessID

deriving instance (Show a, Show b) => Show (DNode a b)

deriving instance (Eq a, Eq b) => Eq (DNode a b)

instance Functor (DNode a) where
    fmap f (DNode a b) = DNode a (f b) 

instance Copointed (DNode a) where
    copoint (DNode a b) = b

instance Foldable (DNode a) where
    foldr f acc (DNode a b) = f b acc

instance Traversable (DNode a) where
    traverse f (DNode a b)  = DNode a <$> f b 

-- | Cross Node

data XNode b = XNode b -- ProcessID

deriving instance (Show b) => Show (XNode b)

deriving instance (Eq b) => Eq (XNode b)

instance Functor XNode where
    fmap f (XNode b) = XNode (f b)

instance Copointed XNode where
    copoint (XNode b) = b

instance Foldable XNode where
    foldr f acc (XNode b) = f b acc
 
instance Traversable XNode where
    traverse f (XNode b) = XNode <$> f b

-- | 

data GDecayTop dnode tnode a b = GDecay (dnode a b, [GDecayTop dnode tnode a b])
                               | GTerminal (tnode a)

deriving instance (Show (dnode a b), Show (tnode a)) => Show (GDecayTop dnode tnode a b)

deriving instance (Eq (dnode a b), Eq (tnode a)) => Eq (GDecayTop dnode tnode a b)

instance (Functor (d a)) => Functor (GDecayTop d t a) where
    fmap f (GDecay (x, xs)) = GDecay (fmap f x, fmap (fmap f) xs)
    fmap f (GTerminal x) = GTerminal x 

instance (Copointed (d a)) => Foldable (GDecayTop d t a) where
    foldr f acc (GTerminal _) = acc 
    foldr f acc (GDecay (x,xs)) = f (copoint x) (foldr (flip (foldr f)) acc xs)   


instance (Traversable (d a), Copointed (d a)) => Traversable (GDecayTop d t a) where
    traverse f (GTerminal x) = GTerminal <$> pure x
    traverse f (GDecay (x, xs)) = GDecay <$> ( (,)  <$> traverse f x 
                                                    <*> sequenceA (map (traverse f) xs) )

-- | 

data GCross xnode dnode tnode a b = 
    GCross { xprogG :: xnode b 
           , incomingG :: [GDecayTop dnode tnode a b] 
           , outgoingG :: [GDecayTop dnode tnode a b]  
           } 

deriving instance (Show (xnode b), Show (dnode a b), Show (tnode a)) => Show (GCross xnode dnode tnode a b)

deriving instance (Eq (xnode b), Eq (dnode a b), Eq (tnode a)) => Eq (GCross xnode dnode tnode a b) 

instance (Functor (d a), Functor x) => Functor (GCross x d t a) where
    fmap f (GCross proc inc out) = GCross (fmap f proc) (fmap (fmap f) inc) (fmap (fmap f) out) 

instance (Copointed (d a), Copointed x) => Foldable (GCross x d t a) where
    foldr f acc (GCross proc inc out) = 
      let minc = foldr (flip (foldr f)) acc inc
          mout = foldr (flip (foldr f)) minc out
      in f (copoint proc) mout

instance (Traversable (d a), Copointed (d a), Copointed x, Traversable x ) => Traversable (GCross x d t a) where
    traverse f (GCross proc inc out) = GCross <$> traverse f proc 
                                              <*> sequenceA (map (traverse f) inc)
                                              <*> sequenceA (map (traverse f) out)



-- | 

type ProcessID = Int

-- | 

type ParticleID = Int

-- | 

type EventMap = M.IntMap LHEvent 


-- | 

type DecayID = GDecayTop DNode TNode (ParticleID,PKind) ProcessID

-- | 

type CrossID = GCross XNode DNode TNode (ParticleID,PKind) ProcessID

-- | 

type DecayFull = GDecayTop DNode TNode (ParticleID,PDGID) ContextMatchedLHEvent

-- | 

type CrossFull = GCross XNode DNode TNode (ParticleID,PDGID) ContextMatchedLHEvent 

-- | 

data MatchedLHEvent = MLHEvent { mlhev_procid :: ProcessID
                               , mlhev_orig :: LHEvent 
                               , mlhev_einfo :: EventInfo
                               , mlhev_incoming :: [(ParticleID,PtlInfo)]
                               , mlhev_outgoing :: [(ParticleID,PtlInfo)]
                               , mlhev_intermediate :: [PtlInfo] } 

data ContextMatchedLHEvent = CMLHEvent { upper :: Maybe (MatchedLHEvent,(ParticleID,PDGID,PtlInfo)) -- ^ particle id is for current particle
                                       , current :: MatchedLHEvent 
                                       } 

-- | 

data AdjustCandidate = AC { ac_move :: PDGID -> PDGID } 

-- | 

getPInfos :: LHEvent -> [PtlInfo]
getPInfos (LHEvent _ ps) = ps 


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
getProcessFromCross (GCross (XNode p) inc out) = 
    p : concatMap getProcessFromDecayTop inc 
    ++ concatMap getProcessFromDecayTop out 

-- | 

getProcessFromDecayTop2 :: GDecayTop DNode TNode a b -> [b] 
getProcessFromDecayTop2 = foldr (:) [] 
  -- where f x acc = x:acc 

-- | 

getProcessFromCross2 :: GCross XNode DNode TNode a b -> [b]
getProcessFromCross2 = foldr (:) []




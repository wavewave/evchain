{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

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

-- | Terminal Node 

data TNode a = TNode a 

-- | Decay Node

data DNode a b = DNode a b -- ProcessID

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
    GCross { incomingG :: [GDecayTop dnode tnode a b] 
           , outgoingG :: [GDecayTop dnode tnode a b]  
           , xprocG :: xnode b } 

instance (Functor (d a), Functor x) => Functor (GCross x d t a) where
    fmap f (GCross inc out proc) = GCross (fmap (fmap f) inc) (fmap (fmap f) out) (fmap f proc)

instance (Copointed (d a), Copointed x) => Foldable (GCross x d t a) where
    foldr f acc (GCross inc out proc) = 
      let minc = foldr (flip (foldr f)) acc inc
          mout = foldr (flip (foldr f)) minc out
      in f (copoint proc) mout

instance (Traversable (d a), Copointed (d a), Copointed x, Traversable x ) => Traversable (GCross x d t a) where
    traverse f (GCross inc out proc) = GCross <$> sequenceA (map (traverse f) inc)
                                              <*> sequenceA (map (traverse f) out)
                                              <*> traverse f proc 


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

type DecayFull = GDecayTop DNode TNode (ParticleID,PDGID) MatchedLHEvent

-- | 

type CrossFull = GCross XNode DNode TNode (ParticleID,PDGID) MatchedLHEvent 

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


getProcessFromDecayTop2 :: GDecayTop DNode TNode a b -> [b] 
getProcessFromDecayTop2 = foldr (:) [] 
  -- where f x acc = x:acc 

getProcessFromCross2 :: GCross XNode DNode TNode a b -> [b]
getProcessFromCross2 = foldr (:) []




{-# LANGUAGE GADTs, EmptyDataDecls, FlexibleInstances, StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}

module HEP.Automation.MadGraph.EventChain.Spec where

import HEP.Parser.LHEParser.Type
import HEP.Automation.MadGraph.EventChain.Type


data PKind = KPDGID PDGID
             deriving (Show,Eq)

instance Num PKind where
  _ + _ = error " no + defined for PKind " 
  _ - _ = error " no - defined for PKind "
  _ * _ = error " no - defined for PKind "
  abs _ = error " no abs defined for PKind " 
  signum _ = error " no signum defined for PKind " 
  fromInteger n = KPDGID (fromInteger n)


type SDecayTop = GDecayTop DNode TNode PKind ProcessID

type SCross = GCross XNode DNode TNode PKind ProcessID 

instance Num (SDecayTop) where
  _ + _ = error " no + defined for SDecayTop"
  -- _ - _ = error " no - defined for SDecayTop"
  negate (GTerminal (TNode (KPDGID n))) = GTerminal (TNode (KPDGID (-n)))
  negate _ = error " no negate defined for SDecayTop in general"
  _ * _ = error " no * defined for SDecayTop"
  abs _ = error " no abs defined for SDecayTop"
  signum _ = error " no signum defined for SDecayTop"  
  fromInteger n = GTerminal (TNode (KPDGID (fromInteger n)))

-- | 

x :: ProcessID -> (SDecayTop,SDecayTop,[SDecayTop]) -> SCross
x procid (a,b,ys) =  GCross (XNode procid) [a,b] ys 

-- |

d :: ProcessID -> (PKind,[SDecayTop]) -> SDecayTop 
d procid (a,ys) = GDecay (DNode a procid,ys)

-- | 

makeDecayID :: ParticleID -> SDecayTop -> DecayID
makeDecayID idnum (GTerminal (TNode (KPDGID n))) = GTerminal (TNode (idnum,n))
makeDecayID idnum (GDecay (DNode (KPDGID n) procid,ys)) = GDecay (DNode (idnum,n) procid,dtable)
  where dtable = zipWith makeDecayID [1..] ys

-- | 

makeCrossID :: SCross -> CrossID
makeCrossID (GCross procid inc out) = GCross procid incnew outnew 
  where incnew = zipWith makeDecayID [1..] inc
        outnew = zipWith makeDecayID [length inc+1..] out

{-
t :: PKind -> SDecayTop 
t a = GTerminal (TNode a)
-}


-- type 

-- |

{-

data TCross
data TDecay
data TTerminal


 
-- | 

data Spec a where
    X :: (PKind,PKind,[PKind]) -> Spec TCross
    D :: (PKind,[PKind])       -> Spec TDecay
    T :: PKind                 -> Spec TTerminal 
--    deriving (Show,Eq)
            
instance Num (Spec TTerminal) where
  _ + _ = error " no + defined for Spec"
  _ - _ = error " no - defined for Spec"
  _ * _ = error " no * defined for Spec"
  abs _ = error " no abs defined for Spec"
  signum _ = error " no signum defined for Spec"  
  fromInteger n = T (KPDGID (fromInteger n))

-- | 


deriving instance (Show (Spec a))

deriving instance (Eq (Spec a))


interpreteD :: Spec TTDecay -> 

-}
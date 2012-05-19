{-# LANGUAGE GADTs, EmptyDataDecls, FlexibleInstances, StandaloneDeriving #-}

module HEP.Automation.MadGraph.EventChain.Spec where

import HEP.Parser.LHEParser.Type
import HEP.Automation.MadGraph.EventChain.Type

{-
-- |

data TCross
data TDecay
data TTerminal


data PKind = KPDGID PDGID
             deriving (Show,Eq)

instance Num PKind where
  x + y = x
  x - y = x 
  x * y = x
  abs x = x 
  signum x = x 
  fromInteger n = KPDGID (fromInteger n)
 
-- | 

data Spec a where
    X :: (PKind,PKind,[PKind]) -> Spec TCross
    D :: (PKind,[PKind])       -> Spec TDecay
    T :: PKind                 -> Spec TTerminal 
--    deriving (Show,Eq)
            
instance Num (Spec TTerminal) where
  x + y = x
  x - y = x 
  x * y = x
  abs x = x 
  signum x = x 
  fromInteger n = T (KPDGID (fromInteger n))

-- | 


deriving instance (Show (Spec TCross))
deriving instance (Show (Spec TDecay))
deriving instance (Show (Spec TTerminal))


deriving instance (Eq (Spec TCross))
deriving instance (Eq (Spec TDecay))
deriving instance (Eq (Spec TTerminal))

-}
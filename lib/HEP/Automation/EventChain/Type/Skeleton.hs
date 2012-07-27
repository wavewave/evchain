{-# LANGUAGE DataKinds, GADTs, KindSignatures, TypeFamilies, 
             ExistentialQuantification, MultiParamTypeClasses, 
             ConstraintKinds, TypeSynonymInstances, FlexibleInstances, 
             FlexibleContexts, RecordWildCards, 
             ScopedTypeVariables, Rank2Types, GeneralizedNewtypeDeriving, 
             StandaloneDeriving #-}
-- ViewPatterns 

-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Automation.EventChain.Type.Skeleton
-- Copyright   : (c) 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- Types needed for event chain utility
--
-----------------------------------------------------------------------------

module HEP.Automation.EventChain.Type.Skeleton where 

-- import GHC.Prim
import           Control.Applicative
import           Data.Foldable
import           Data.Traversable
-- import qualified Data.Vec as Vec
-- import           Data.Hashable 


import Prelude hiding (foldr)

-- | 

instance Foldable ((,) b) where
  foldr f acc (_,y) = f y acc 

-- | 

instance Traversable ((,) b) where
  traverse f (x,y) = (,) x  <$> f y

-- | 

data Cross x d t = MkC { xnode :: x 
                       , xincs :: (Decay d t, Decay d t)  -- ^ two incoming particle trees
                       , xouts :: [Decay d t] }           -- ^ a list of outgoing particle trees 

deriving instance (Show x, Show d, Show t) => Show (Cross x d t)

deriving instance (Eq x,Eq d, Eq t) => Eq (Cross x d t)

{-
instance (Hashable x, Hashable d, Hashable t) => Hashable (Cross x d t) where 
  hash MkC {..} = hash xnode `hashWithSalt` xincs `hashWithSalt` xouts 
  hashWithSalt s MkC {..} = s `hashWithSalt` xnode `hashWithSalt` xincs `hashWithSalt` xouts 
-}

-- | 

data Decay d t = MkD { dnode :: d   
                     , douts :: [Decay d t] } 
               | MkT { tnode :: t }

deriving instance (Show d, Show t) => Show (Decay d t) 

deriving instance (Eq d, Eq t) => Eq (Decay d t) 

{-
instance (Hashable d, Hashable t) => Hashable (Decay d t) where
  hash MkD {..} = 0 `hashWithSalt` dnode `hashWithSalt` douts 
  hash MkT {..} = 1 `hashWithSalt` tnode 
  hashWithSalt s MkD {..} = s `combine` 0 `hashWithSalt` dnode `hashWithSalt` douts 
  hashWithSalt s MkT {..} = s `combine` 1 `hashWithSalt` tnode 
-}

-- | general pullback definition

class PullBackable l l' u u' where
  pullback :: (u -> u') -> l -> l' 
  pullbackA :: (Applicative f) => (u -> f u') -> l -> f l'

---------------------------------------------------------------------------
--   DecayA, CrossA                                                      --
---------------------------------------------------------------------------

-- | newtype wrapper of Decay to define Functor instance over all node when x=d=t=a  

newtype DecayA a = DecayA { unDecayA :: Decay a a }

instance PullBackable (Decay a a) (Decay b b) (DecayA a) (DecayA b) where
  pullback f = unDecayA . f . DecayA
  pullbackA f = liftA unDecayA . f . DecayA

instance Functor DecayA where
  fmap f (DecayA (MkD {..})) = DecayA ( MkD (f dnode) 
                                            (fmap (lift (fmap f)) douts) )
    where lift = pullback :: (DecayA a -> DecayA b) -> Decay a a -> Decay b b 
  fmap f (DecayA (MkT {..})) = DecayA ( MkT (f tnode))

instance Foldable DecayA where
  foldr f acc (DecayA (MkT {..})) = f tnode acc
  foldr f acc (DecayA (MkD {..})) = f dnode (foldr accumf acc douts) 
    where accumf x ac = foldr f ac (DecayA x)

instance Traversable DecayA where
  traverse f (DecayA (MkT {..})) = DecayA <$> (MkT <$> f tnode)
  traverse f (DecayA (MkD {..})) =
      DecayA <$> ( MkD <$> f dnode <*> sequenceA (map (lift (traverse f)) douts) ) 
    where lift = pullbackA :: (Applicative f) => (DecayA a -> f (DecayA b))-> Decay a a -> f (Decay b b)

-- | newtype wrapper of Decay to define Functor instance over all node when x=d=t=a

newtype CrossA a = CrossA { unCrossA :: Cross a a a } 

instance PullBackable (Cross a a a) (Cross b b b) (CrossA a) (CrossA b) where
  pullback f = unCrossA . f . CrossA
  pullbackA f = liftA unCrossA . f . CrossA

instance Functor CrossA where
  fmap f (CrossA (MkC {..})) 
      = CrossA ( MkC (f xnode) (lift (fmap f) in1, lift (fmap f) in2)
                     (fmap (lift (fmap f)) xouts ) )

    where in1 = fst xincs
          in2 = snd xincs 
          lift = pullback :: (DecayA a -> DecayA b) -> Decay a a -> Decay b b 

instance Foldable CrossA where  
  foldr f acc (CrossA (MkC {..})) = f xnode accs 
    where accumf x ac = foldr f ac . DecayA $ x
          acc1 = accumf (fst xincs) acc 
          acc2 = accumf (snd xincs) acc1 
          accs = foldr accumf acc2 xouts 

instance Traversable CrossA where
  traverse f (CrossA (MkC {..})) = CrossA <$> mcross 
    where (xin1,xin2) = xincs 
          mxincs = (,) <$> travf xin1 <*> travf xin2  
          mxouts = sequenceA (map travf xouts)
          mcross= MkC <$> f xnode <*> mxincs <*> mxouts 
          travf = liftA unDecayA . traverse f . DecayA 
         


-- | to a single-typed node tree
 
toDecayA :: (d -> a) -> (t -> a) -> Decay d t -> Decay a a 
toDecayA df tf MkD {..} = MkD (df dnode) (fmap (toDecayA df tf) douts)
toDecayA _df tf MkT {..} = MkT (tf tnode) 

-- | to a single-typed node tree

toCrossA :: (x -> a) -> (d -> a) -> (t -> a) -> Cross x d t -> Cross a a a 
toCrossA xf df tf MkC {..} = MkC (xf xnode) (toDecayA df tf (fst xincs), toDecayA df tf (snd xincs)) 
                                 (fmap (toDecayA df tf) xouts) 

-- Traversable


---------------------------------------------------------------------------
--   DecayF, CrossF                                                      --
---------------------------------------------------------------------------

-- | newtype wrapper of Decay to define Functor instance over process type 

newtype DecayF b a = DecayF { unDecayF :: Decay (b,a) b } 

instance PullBackable (Decay (b,a) b) (Decay (d,c) d) (DecayF b a) (DecayF d c) where
  pullback f = unDecayF . f . DecayF 
  pullbackA f = liftA unDecayF .  f  . DecayF 

instance Functor (DecayF b) where
  fmap f (DecayF (MkD {..})) = DecayF ( MkD (fmap f dnode) 
                                            (fmap (lift (fmap f)) douts) )
    where lift = pullback :: (DecayF b a -> DecayF d c) -> Decay (b,a) b -> Decay (d,c) d
  fmap _ (DecayF (MkT {..})) = DecayF (MkT tnode) 

instance Foldable (DecayF b) where
  -- foldr :: forall a c. (a -> c->c ) -> c -> (DecayF b a) -> c
  foldr _ acc (DecayF (MkT {..})) = acc 
  foldr f acc (DecayF (MkD {..})) = f (snd dnode) 
                                      (foldr accumf acc douts)
    where -- accumf :: Decay (b,a) b -> c -> c 
          accumf x ac = foldr f ac (DecayF x)

instance Traversable (DecayF b) where
  traverse _ (DecayF (MkT {..})) = DecayF <$> (MkT <$> pure tnode)
  traverse f (DecayF (MkD {..})) = 
    DecayF <$> ( MkD <$> traverse f dnode 
                     <*> sequenceA (map (liftA unDecayF . traverse f . DecayF ) douts) )



-- | newtype wrapper of Cross to define Functor instance over process type

newtype CrossF b a = CrossF { unCrossF :: Cross a (b,a) b }

instance PullBackable (Cross a (b,a) b) (Cross c (d,c) d) (CrossF b a) (CrossF d c) where
  pullback f = unCrossF . f . CrossF
  pullbackA f = liftA unCrossF . f . CrossF


instance Functor (CrossF b) where
  fmap f (CrossF (MkC {..})) 
      = CrossF ( MkC (f xnode) 
                     (lift (fmap f) (fst xincs), lift (fmap f) (snd xincs)) 
                     (fmap (lift (fmap f)) xouts) )  
    where lift = pullback :: (DecayF b a -> DecayF d c) -> Decay (b,a) b -> Decay (d,c) d
  
instance Foldable (CrossF b) where
  -- foldr :: forall a c. (a->c->c) -> c -> (DecayF b a) -> c
  foldr f acc (CrossF (MkC {..})) = f xnode accs 
    where -- accumf :: Decay (b,a) b -> c -> c
          accumf x ac = foldr f ac . DecayF $ x 
          acc1 = accumf (fst xincs) acc
          acc2 = accumf (snd xincs) acc1
          accs = foldr accumf acc2 xouts 

instance Traversable (CrossF b) where
  traverse f (CrossF (MkC {..})) = CrossF <$> mcross
    where (xin1,xin2) = xincs 
          mxincs = (,) <$> travf xin1 <*> travf xin2  
          mxouts = sequenceA (map travf xouts)
          mcross= MkC <$> f xnode <*> mxincs <*> mxouts 
          travf = liftA unDecayF . traverse f . DecayF 

-----------------------------------------------------------------------------
-- useful functions 
-----------------------------------------------------------------------------

-- | bifunctor 
 
fmapD :: (d1 -> d2) -> (t1 -> t2) -> Decay d1 t1 -> Decay d2 t2
fmapD df tf MkD {..} = MkD (df dnode) (fmap (fmapD df tf) douts)
fmapD _df tf MkT {..} = MkT (tf tnode) 

-- | trifunctor 

fmapX :: (x1 -> x2) -> (d1 -> d2) -> (t1 -> t2) 
      -> Cross x1 d1 t1 -> Cross x2 d2 t2 
fmapX xf df tf MkC {..} = MkC (xf xnode) 
                            (fmapD df tf (fst xincs), fmapD df tf (snd xincs)) 
                            (fmap (fmapD df tf) xouts) 

-- | 

getNodeD :: Decay a a -> a 
getNodeD MkD {..} = dnode 
getNodeD MkT {..} = tnode 










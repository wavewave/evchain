{-# LANGUAGE GADTs, DataKinds, KindSignatures, ExistentialQuantification #-}


-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Automation.EventChain.Type.CVec
-- Copyright   : (c) 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- Vector with type-safe length (Peano number) and arbitrary length 
-- collection 
--
-----------------------------------------------------------------------------

module HEP.Automation.EventChain.Type.CVec where

-- | Natural number kind (using DataKinds )

data Nat = Z | S Nat  

-- | vector of type-safe length

data Vec (n :: Nat) a where
    Nil  :: Vec Z a 
    Cons :: a -> Vec n' a -> Vec (S n') a

-- | collection wrapper of vector

data CVec a = forall n. MkCVec (Vec n a)

testcvec :: Vec (S Z) Int 
testcvec = 1 `Cons` Nil 

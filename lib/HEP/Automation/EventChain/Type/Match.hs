{-# LANGUAGE ScopedTypeVariables, StandaloneDeriving, RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Automation.EventChain.Type.Match
-- Copyright   : (c) 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- Utility functions for matching a lhe format data with a given spec 
-- 
-----------------------------------------------------------------------------

module HEP.Automation.EventChain.Type.Match where

-- from other packages from others 
import           Control.Applicative
import           Control.Monad.Error
import           Control.Monad.State
import qualified Data.Map as M
-- from other hep-platform package
import           HEP.Parser.LHEParser.Type (PtlInfo,PtlID,PDGID,LHEvent(..),EventInfo)
import           HEP.Util.Functions
-- from this package
import           HEP.Automation.EventChain.Type.Process
import           HEP.Automation.EventChain.Type.Skeleton
import           HEP.Automation.EventChain.Type.Spec



-- | Matching Monad 
type MatchM m = ErrorT String (StateT [PtlInfo] m) 
 

-- | 
data MatchInOut p = 
  MIO { mio_incoming :: [Either (ParticleID,PtlInfo) (ParticleID,(p,PtlInfo))]
      , mio_outgoing :: [Either (ParticleID,PtlInfo) (ParticleID,(p,PtlInfo))]
      , mio_remaining :: [PtlInfo]
      } 

-- | data type for a single LHE event matched with a specified process node 
data MatchedLHEvent p tnode dnode = 
         MLHEvent { mlhev_procinfo :: p
                  , mlhev_orig :: LHEvent 
                  , mlhev_einfo :: EventInfo
                  , mlhev_incoming :: [(Either tnode dnode, PtlInfo)]
                  , mlhev_outgoing :: [(Either tnode dnode, PtlInfo)]
                  , mlhev_intermediate :: [PtlInfo] } 


deriving instance (Show p, Show tnode, Show dnode) 
         => Show (MatchedLHEvent p tnode dnode)

-- -- | default type for MatchedLHEvent 

-- type MatchedLHEventSimple = MatchedLHEvent ParticleID 

getPInfos :: LHEvent -> [PtlInfo]
getPInfos (LHEvent _ pinfos) = pinfos 

-- | MatchedLHEvent with Decay structure 
type MatchedLHEventProcess p = MatchedLHEvent p ParticleID (ParticleID,p) 


-- | 
newtype MatchedLHEventProcessF p = 
    MkMLHEPF { unMkMLHEPF :: MatchedLHEventProcess p} 

instance Functor MatchedLHEventProcessF where
  fmap f (MkMLHEPF MLHEvent {..}) = 
    MkMLHEPF (MLHEvent (f mlhev_procinfo) mlhev_orig mlhev_einfo
                           (map (fmap' f) mlhev_incoming)
                           (map (fmap' f) mlhev_outgoing)
                           mlhev_intermediate )
    where fmap' f (Left x,y) = (Left x,y)
          fmap' f (Right (x,p),y) = (Right (x,f p),y)



-- | 
data PTriplet = PTriplet { pt_pid :: ParticleID
                         , pt_pdgid :: PDGID
                         , pt_pinfo :: PtlInfo } 

-- | data type for event and context for a node 
data ContextEvent p = 
    CEvent 
    { absoluteContext :: LorentzRotation -- ^ relative to cross frame 
    , relativeContext :: Maybe (p, PTriplet) -- ^ relative to mother 
    , selfEvent :: MatchedLHEventProcess p }


instance Functor ContextEvent where 
  fmap f CEvent {..} = 
      let rel' = case relativeContext of 
                   Nothing -> Nothing 
                   Just (p,trip) -> Just (f p, trip) 
          self' = unMkMLHEPF . fmap f . MkMLHEPF $ selfEvent
      in CEvent absoluteContext rel' self'

-- :: Either ParticleID (ParticleID,p) -> Either ParticleID (ParticleID,f p)
           

 
-- | full decay info
type DecayFull p = Decay ((ParticleID,PDGID),ContextEvent p) (ParticleID,PDGID) 

-- | full cross info 
type CrossFull p = Cross (ContextEvent p) 
                         ((ParticleID,PDGID),ContextEvent p) 
                         (ParticleID,PDGID) 




-- | coordinate for a particle in a given collection of processes 
type ParticleCoord = (ProcessIndex,ParticleID)

-- | coord storage for particles  
type ParticleCoordMap = M.Map ParticleCoord PtlID 




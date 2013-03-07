{-# LANGUAGE ScopedTypeVariables, RecordWildCards, Rank2Types, ExistentialQuantification #-}

-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Automation.EventChain.File
-- Copyright   : (c) 2012,2013 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- File operation 
-- 
-----------------------------------------------------------------------------

module HEP.Automation.EventChain.File where

-- from other packages 
import           Data.Conduit
import           Data.Conduit.Binary
import qualified Data.Conduit.List as CL
import           Data.Conduit.Zlib
import qualified Data.Traversable as T
import           System.IO
import           Text.XML.Stream.Parse
-- from other hep-platform packages 
import           HEP.Parser.LHE.Conduit
import           HEP.Parser.LHE.Type
-- from this package
import           HEP.Automation.EventChain.Type.Process


-- | get parsed LHEvent from a ungzipped lhe file. 
evtsHandle :: Bool  -- ^ is zipped 
           -> Handle 
           -> Source IO LHEvent
evtsHandle p h 
    | p         = sourceHandle h =$= ungzip =$= parseBytes def =$= parseEvent
    | otherwise = sourceHandle h =$= parseBytes def =$= parseEvent 


-- | 
getLHEvents :: FilePath -> IO [LHEvent] 
getLHEvents fn = do 
  h <- openFile fn ReadMode 
  evts <- evtsHandle True h $$ CL.consume 
  return evts 

-- | 
makeLHEProcessMap :: ProcessMap FilePath -> IO (ProcessMap [LHEvent])
makeLHEProcessMap = T.mapM getLHEvents 


{-# LANGUAGE ScopedTypeVariables, RecordWildCards, Rank2Types, ExistentialQuantification #-}

module HEP.Automation.EventChain.FileDriver where

-- from other packages 
import           Data.Conduit
import           Data.Conduit.Binary
import qualified Data.Conduit.List as CL
import           Data.Conduit.Zlib
import           Data.Maybe 
import           System.IO
import           Text.XML.Stream.Parse
-- from other hep-platform packages 
import           Data.Conduit.Util.Count
import           HEP.Util.Count 
-- from this package
import           HEP.Parser.LHEParser.Type
import           HEP.Parser.LHEParser.Parser.Conduit


-- | get parsed LHEvent from a ungzipped lhe file. 

evtsHandle :: Bool  -- ^ is zipped 
           -> Handle 
           -> Source IO (Maybe LHEvent)
evtsHandle f h 
    | f     = sourceHandle h =$= ungzip =$= parseBytes def =$= parseEvent
    | not f = sourceHandle h =$= parseBytes def =$= parseEvent 




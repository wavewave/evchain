{-# LANGUAGE ScopedTypeVariables, RecordWildCards, Rank2Types, ExistentialQuantification #-}

module HEP.Automation.EventChain.FileDriver where

-- from other packages 
import           Data.Conduit
import           Data.Conduit.Binary
import qualified Data.Conduit.List as CL
import           Data.Conduit.Zlib
import           Data.Maybe 
import qualified Data.Traversable as T
import           System.IO
import           Text.XML.Stream.Parse
-- from other hep-platform packages 
import           Data.Conduit.Util.Count
import           HEP.Util.Count 
import           HEP.Parser.LHEParser.Type
import           HEP.Parser.LHEParser.Parser.Conduit
-- from this package
import           HEP.Automation.EventChain.Type.Process

-- | get parsed LHEvent from a ungzipped lhe file. 

evtsHandle :: Bool  -- ^ is zipped 
           -> Handle 
           -> Source IO (Maybe LHEvent)
evtsHandle f h 
    | f     = sourceHandle h =$= ungzip =$= parseBytes def =$= parseEvent
    | not f = sourceHandle h =$= parseBytes def =$= parseEvent 


-- | 
getLHEvents :: FilePath -> IO [LHEvent] 
getLHEvents fn = do 
  h <- openFile fn ReadMode 
  evts <- evtsHandle True h =$= CL.map fromJust $$ CL.consume 
  return evts 


-- | 
makeLHEProcessMap :: ProcessMap FilePath -> IO (ProcessMap [LHEvent])
makeLHEProcessMap = T.mapM getLHEvents 


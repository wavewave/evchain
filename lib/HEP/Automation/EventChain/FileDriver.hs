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



{-

import Control.Monad.State
import Control.Monad.Trans


import Data.Conduit as C
import qualified Data.Conduit.List as CL 
import Data.Conduit.Binary -- hiding (openFile)
-- import Data.Enumerator.IO
import Data.Conduit.Util.Control
import Data.Conduit.Util.Count
-- import Data.Enumerator.Trans
import HEP.Util.Count
import System.IO
import Data.XML.Types

import Text.XML.Stream.Parse

import HEP.Parser.LHEParser.Parser.Conduit
import HEP.Parser.LHEParser.Type
import HEP.Parser.LHEParser.DecayTop

import Text.XML.Conduit.Parse.Util

-- import HEP.Automation.EventAnalysis.Print



-- import HROOT

type DecayTopSink a b m c = Sink (Maybe (a,b,[DecayTop PtlIDInfo])) m c 


-- | 
{-
zipSinks3 :: Monad m => Sink i m r -> Sink i m r' -> Sink i m r'' -> Sink i m (r,r',r'') 
zipSinks3 s1 s2 s3 = fmap (\((x,y),z) -> (x,y,z)) (s1 `CL.zipSinks` s2 `CL.zipSinks` s3)
-}
-- | 

ungzipHandle :: (MonadIO m, MonadUnsafeIO m, MonadThrow m) => Handle -> Source m Event
ungzipHandle h = sourceHandle h =$= ungzip =$= parseBytes def

-- | 

-- :: DecayTopSink LHEvent PtlInfoMap CountIO c -> Conduit Event CountIO (Int,(),c)

lheventIter :: Monad m => Conduit Event m (Maybe (LHEvent, PtlInfoMap, [DecayTop PtlIDInfo]))
lheventIter = parseEvent =$= decayTopConduit =$= ordDecayTopConduit

{- action = do 
  let process = zipSinks3 countIter countMarkerIter action
  header <- textLHEHeader
  parseEvent =$= decayTopConduit =$= ordDecayTopConduit =$ process
-}

-- | 
{-
doSingleFileAnalysis :: SingleFileAnalysis -> IO ()
doSingleFileAnalysis SingleFileAnalysisDraw1DHistFromLHE{..} = do 
    h <- openFile datafile ReadMode
    ungzipHandle h =$= lheventIter $$ hist1dfunc hist1d
    hClose h 
doSingleFileAnalysis SingleFileAnalysisCountingLHE{..} = do 
    h <- openFile datafile ReadMode 
    ungzipHandle h =$= lheventIter $$ countfunc
    hClose h
-}

{- 
-- | deprecated 

doReadXmlOnly :: FilePath -> IO ()
doReadXmlOnly fp = do 
    let process = zipSinks3 countIter countMarkerIter (showSomeEvents 30) -- (hist1dfunc hist1d)  
        iter = do 
          header <- textLHEHeader
          parseEvent $ decayTopConduit =$ process
    r <- processFile iter fp 
    putStrLn $ show r 
    return ()
-}

-- | 

{-
data SingleFileAnalysis =  
  SingleFileAnalysisDraw1DHistFromLHE 
  { datafile :: FilePath
  , hist1d :: TH1D
  , hist1dfunc :: forall a b m. (Show a, MonadIO m) => 
                  TH1D -> Sink (Maybe (a,b,[DecayTop PtlIDInfo])) m () 
  }
  | SingleFileAnalysisCountingLHE
  { datafile :: FilePath
  , countfunc :: forall a b m. (MonadIO m) => DecayTopSink a b m () 
  }
-}

-}
{-# LANGUAGE RecordWildCards #-}

module Test.EventChain.Match where

-- from other packages from others
import           Control.Monad.Error
import           Control.Monad.Identity
import           Control.Monad.State
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.Either (either)
import           Data.Maybe (catMaybes,isJust)
import           System.Directory
import           System.FilePath
import           System.IO
-- from other hep-platform packages 
import           HEP.Parser.LHEParser.Type
-- from this package 
import           HEP.Automation.EventChain.FileDriver 
import           HEP.Automation.EventChain.Match 
import           HEP.Automation.EventChain.Type.Match
import           HEP.Automation.EventChain.Type.Spec
import           HEP.Automation.EventChain.SpecDSL
-- 
import           Paths_evchain
import           Test.EventChain.Common 
import           Debug.Trace 

-- | 

test_match_driver :: ErrorT String IO () 
test_match_driver = lift test_lhe >>= test_matchs

-- | 

test_lhe :: IO LHEvent 
test_lhe = do 
  dir <- getDataDir 
  let fn = dir </> "test" </> "resources" </> "pp_gogo_events.lhe"
  h <- openFile fn ReadMode
  evtsHandle False h $$ CL.consume >>= return.head.catMaybes 
 

-- | 

test_matchs :: LHEvent -> ErrorT String IO ()
test_matchs ev = do 
  guardMsg "fail in test_match" (return . test_match $ ev)
  guardMsg "fail in test_match_single" (return . test_match_single $ ev)
  guardMsg "fail in test_match_multi" (return . test_match_multi $ ev)
  -- guardMsg "fail in test_matchX" (return . test_matchX $ ev)
  -- guardMsg "fail in test_match4" (return . test_match4 $ ev)


-- |

test_match :: LHEvent -> Bool 
test_match ev = length (lhe_ptlinfos ev) == 4 


-- | 

test_match_single :: LHEvent -> Bool 
test_match_single LHEvent {..} = 
  let r = runIdentity (evalStateT (runErrorT (match1 Out 1000021)) lhe_ptlinfos)
  in either (const False) (const True) r 
  
-- | 

test_match_multi :: LHEvent -> Bool
test_match_multi LHEvent {..} = 
  let m = matchOr "test_match_multi" (match1 Out) [1,2,3,100,1000021] 
      r = runIdentity (evalStateT (runErrorT m) lhe_ptlinfos)
  in trace (show r) $ either (const False) (const True) r 

-- | 
{-

spec_gluglugogo :: CrossID
spec_gluglugogo = fmapX id

                  xp 1 (21,21,[1000021,1000021]) 
  where dfunc :: [
 
-- | 

test_matchX :: LHEvent -> Bool 
test_matchX ev = let r = runIdentity (matchX spec_gluglugogo) ev 
                 in trueOrFalse r 

-}

{-
-- | 

test_selector :: SelectFunc
test_selector = SelectFunc f "outgoing gluino"
  where f (1000021,1) = True 
        f _ = False 


-- | 

test_selectorlst :: [(ParticleID,SelectFunc)]
test_selectorlst = [(3,test_selector),(4,test_selector)] 

-- | 

test_gluon :: SelectFunc 
test_gluon = SelectFunc f "incoming gluon" 
  where f (21,-1) = True 
        f _ = False 

-- | 

test_selectinc = [(1,test_gluon),(2,test_gluon)]


-- | 

test_match2 :: LHEvent -> Bool 
test_match2 ev = either (const False) (const True)
                   (evalState (runErrorT (findPtlBy test_selector)) (lhe_ptlinfos ev)) 

-- | 

test_match3 :: LHEvent -> Bool 
test_match3 ev = either (const False) (const True) 
                   (evalState (runErrorT (findPtls test_selectorlst)) (lhe_ptlinfos ev))

-- | 


test_match4 :: LHEvent -> Bool 
test_match4 ev = either (\msg -> trace msg False) (const True) matched

  where matched = matchInOut 0 (test_selectinc,test_selectorlst) ev 
      
-}


{-# LANGUAGE TupleSections, NoMonomorphismRestriction #-}

module Main where


import Control.Monad.State

import Data.List 
import qualified Data.IntMap as M

import HEP.Parser.LHEParser.Parser.Conduit
import HEP.Automation.MadGraph.EventChain.FileDriver 


import HEP.Util.Count 

import HEP.Parser.LHEParser.Type 


import System.IO

import           Data.Conduit -- hiding (sequence)
import qualified Data.Conduit.List as CL
import           Data.Conduit.Util.Control as CU (zipStreamWithList,zipSinks3,dropWhile,takeFirstN,zipN,sequence) 
import           Data.Conduit.Util.Count


import HEP.Automation.MadGraph.EventChain.Type
import HEP.Automation.MadGraph.EventChain.LHEConn
import HEP.Automation.MadGraph.EventChain.Print 
import HEP.Automation.MadGraph.EventChain.Spec
import HEP.Automation.MadGraph.EventChain.Util

import Prelude hiding (dropWhile)

{-
-- | 

zipWithM3 :: (Monad m) => (a -> b -> c -> m d) -> [a] -> [b] -> [c] -> m [d]
zipWithM3 f xs ys zs = sequence (zipWith3 f xs ys zs)


-- | 

zipWithM5 :: (Monad m) => (a -> b -> c -> d -> e -> m f) 
          -> [a] -> [b] -> [c] -> [d] -> [e] -> m [f]
zipWithM5 f x1s x2s x3s x4s x5s = sequence (zipWith5 f x1s x2s x3s x4s x5s)

-}

-- | 

evtFrmFileSrc :: Int -> Handle -> Source CountIO (Int,LHEvent)
evtFrmFileSrc procid h = ungzipHandle h =$= lheventIter =$= CL.map (f procid) 
  where f procid (Just (lhe,_,_)) = (procid,lhe)



-- | 

assocLHEventWDecayTop :: M.IntMap LHEvent -> IO ()
assocLHEventWDecayTop m = do
    let rmatch = matchFullCross m (makeCrossID ppgogo)
    case rmatch of
      Left err-> do
        error err
      Right fcross -> do 
        pinfos <- accumTotalEvent fcross 
        putStrLn $ lheFormatOutput $ LHEvent (EvInfo 0 0 0 0 0 0) pinfos 
        return ()
   
-- | 

assocLHEFileWDecayTop :: M.IntMap FilePath -> IO ()
assocLHEFileWDecayTop m = do 
    let lst = M.toList m 
    srcs <- mapM getsrc lst  
    let srcfunc = CU.zipN srcs =$= CL.map M.fromList 
    evalStateT ( srcfunc $$ sinkfunc ) (0 :: Int)
    return ()
  where getsrc (k,fname) = do h <- openFile fname ReadMode  
                              return (evtFrmFileSrc k h)

        sinkfunc = CL.mapM_ (liftIO . assocLHEventWDecayTop)


-- | 

ppgogo :: SCross 
ppgogo = x 1 (jet,jet,[gojjn1decay1,1000021])
-- ppgogo = x 1 (jet,jet,[gojjn1decay1,gojjn1decay2])

-- | 

gojjn1decay1 :: SDecayTop
gojjn1decay1 = d 2 (1000021,[jet,jet,n1jjjxdecay1])

-- | 

gojjn1decay2 :: SDecayTop
gojjn1decay2 = d 3 (1000021,[jet,jet,1000022 ]) -- n1jjjxdecay2])

-- | 

n1jjjxdecay1 :: SDecayTop
n1jjjxdecay1 = d 4 (1000022,[ppair 1, ppair 1, ppair 2, ppair 9000201])

-- | 

n1jjjxdecay2 :: SDecayTop 
n1jjjxdecay2 = d 5 (1000022,[ppair 1, ppair 1, ppair 2, ppair 9000201])



-- | 

filenamemap :: M.IntMap FilePath
filenamemap = M.fromList [ (1,"pp_gogo_events.lhe.gz")
                         , (2,"go_n1jj_run01_events.lhe.gz")
                         , (4,"n1_jjjsxxp_run01_events.lhe.gz") ]
                         -- , (3,"go_n1jj_run02_events.lhe.gz") ] 
                         -- , (5,"n1_jjjsxxp_run02_events.lhe.gz") ] 





-- | 

main :: IO ()
main = do 
  assocLHEFileWDecayTop filenamemap


{-    let lhefile1 = "pp_gogo_events.lhe.gz"
        lhefile2 = "go_n1jj_run01_events.lhe.gz"
        lhefile3 = "go_n1jj_run02_events.lhe.gz"
        lhefile4 = "n1_jjjsxxp_run01_events.lhe.gz"
        lhefile5 = "n1_jjjsxxp_run02_events.lhe.gz"
    h1 <- openFile lhefile1 ReadMode
    h2 <- openFile lhefile2 ReadMode
    h3 <- openFile lhefile3 ReadMode
    h4 <- openFile lhefile4 ReadMode
    h5 <- openFile lhefile5 ReadMode

    let s1 = evtFrmFileSrc 1 h1 
        s2 = evtFrmFileSrc 2 h2
        s3 = evtFrmFileSrc 3 h3 
        s4 = evtFrmFileSrc 4 h4 
        s5 = evtFrmFileSrc 5 h5 

    x <- evalStateT (zipN [s1,s2,s3,s4,s5] =$= CL.map M.fromList 
                     $$ CL.mapM_ (liftIO . assocLHEventWDecayTop) ) (0 :: Int)
    print x 
    return () -}

--    zipN [CL.sourceList [1,2,3], CL.sourceList[4,5,6], CL.sourceList [7,8,9,10]] $$ printIter 
{-
    withFile "go_n1jj_run01_events.lhe.gz" ReadMode $ \ih -> do 
      x <- evalStateT (evtFrmFileSrc 1 ih $$ printIter) (0 :: Int)
      putStrLn (show x )
      return ()
-}




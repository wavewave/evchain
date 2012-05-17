module Main where

import System.Console.CmdArgs

import Data.List 
import qualified Data.IntMap as M

import HEP.Parser.LHEParser.DecayTop
import HEP.Automation.EventAnalysis.FileDriver 
import HEP.Automation.EventAnalysis.Print 
import HEP.Util.Functions

import HEP.Parser.LHEParser.Type 

import System.IO

import Data.Enumerator ((=$))
import Data.Enumerator.Util (zipStreamWithList)
import qualified Data.Enumerator.List as EL

import HEP.Automation.MadGraph.EventChain.Type
import HEP.Automation.MadGraph.EventChain.LHEConn


-- | 

zipWithM3 :: (Monad m) => (a -> b -> c -> m d) -> [a] -> [b] -> [c] -> m [d]
zipWithM3 f xs ys zs = sequence (zipWith3 f xs ys zs)


-- | 



testCross :: CrossID -- GCross XNode DNode TNode (ParticleID,PDGID)
testCross = GCross incoms outgos (XNode 1) 

incoms = [ GTerminal (TNode (1,21)) 
         , GTerminal (TNode (2,21)) ] 

outgos = [ GTerminal (TNode (1,-1))
         , GTerminal (TNode (2,1))
         , GTerminal (TNode (3,1000022))
         , GTerminal (TNode (4,-1))
         , GTerminal (TNode (5,1))
         , GTerminal (TNode (6,1000022)) ] 

outgos2 = [ GTerminal (TNode (1,-1))
          , GTerminal (TNode (2,1))
          , GTerminal (TNode (3,1000022))
          , GTerminal (TNode (4,-1))
          , GTerminal (TNode (5,1))
          , testDecay ]


testCross3 = GCross incoms outgos2 (XNode 1)

testDecay :: DecayID -- GDecayTop DNode TNode (ParticleID,PDGID)
testDecay = GDecay (DNode (1,1000022) 33, [ (GTerminal (TNode (1,-2)))
                                          , (GTerminal (TNode (2,-1)))
                                          , (GTerminal (TNode (3,-1)))
                                          , (GTerminal (TNode (4,9000202))) ])

testDecay2 :: DecayID -- GDecayTop DNode TNode (ParticleID,PDGID)
testDecay2 = GDecay (DNode (1,3999) 12, [ (GTerminal (TNode (1,33)))
                                        , testDecay ])


testCross2 :: CrossID -- GCross XNode DNode TNode (ParticleID,PDGID)
testCross2 = GCross incoms [ GTerminal (TNode (1,-1))
                           , testDecay2 ] 
                    (XNode 399)

-- | 

main :: IO () 
main = do 
  let lhefile1 = "test.lhe.gz"
      lhefile2 = "test2.lhe.gz"
  -- putStrLn (printCrossStr testCross)
  -- startMine "test.lhe.gz" "test2.lhe.gz" "test3.lhe.gz"
  --           "testoutput.dat"
  ((_,_,lst1),_) <- processFile 
                      (lheventIter $ zipStreamWithList [1..] =$ iter )
                      lhefile1       
  ((_,_,lst2),_) <- processFile 
                      (lheventIter $ zipStreamWithList [1..] =$ iter )
                      lhefile2      

  let Just (_, Just (fstev,_,_)) = head lst1 
      Just (_, Just (fstev2,_,_)) = head lst2
      LHEvent einfo pinfos = fstev
  putStrLn $ lheFormatOutput fstev
  -- let (Just pinfo1, pinfos') = (findPtlIDStatus (1000021,2) pinfos)
  -- putStrLn $ pformat pinfo1
  -- putStrLn $ concatMap pformat pinfos'

  let r = allFindPtlIDStatus [ (1,-1,1)
                             , (2,1,1)
                             , (3,1000022,1) 
                             , (4,-1,1)
                             , (5,1,1)
                             , (6,1000022,1)
                             ]  
                             pinfos
  case r of 
    Left err -> putStrLn err
    Right (lst,rem) -> do 
      putStrLn (concatMap (\(x,y) -> show x ++ ":" ++ pformat y) lst)
      putStrLn (concatMap pformat rem)
  putStrLn "==============="
  case matchPtl4Cross testCross fstev of
    Left str -> putStrLn str
    Right (MLHEvent _ lst1 lst2 lst3) -> do 
      putStrLn (concatMap (\(x,y) -> show x ++ ":" ++ pformat y) lst1)
      putStrLn (concatMap (\(x,y) -> show x ++ ":" ++ pformat y) lst2)
      putStrLn (concatMap pformat lst3)
  
  let GDecay (x,xs) = testDecay 

  case matchPtl4Decay (x,xs) fstev of
    Left str -> putStrLn str
    Right (MLHEvent _ lst1 lst2 lst3) -> do 
      putStrLn (concatMap (\(x,y) -> show x ++ ":" ++ pformat y) lst1)
      putStrLn (concatMap (\(x,y) -> show x ++ ":" ++ pformat y) lst2)
      putStrLn (concatMap pformat lst3)
  putStrLn "=====" 
  -- putStrLn $ show (getProcessFromDecayTop2 testDecay2)
  putStrLn $ show (getProcessFromCross2 testCross3)
  let tmpmap = M.fromList [(1,fstev),(33,fstev2)]
      rmatch = matchFullCross tmpmap testCross3 
  case rmatch of
    Left err-> putStrLn err
    Right fcross -> do 
      putStrLn $ "number of particles = " ++ show (countPtls fcross)
      putStrLn $ concatMap pformat (accumLHEvent fcross)


-- putStrLn $ intercalate "\n" (map lheFormatOutput lst) 
 where iter = EL.foldM (\lst a -> maybe (return lst) 
                                         (\(n,x) -> return (a:lst) ) a)
                       []

-- | 

startMine :: FilePath  -- ^ main hard process
          -> FilePath  -- ^ decay process1
          -> FilePath  -- ^ decay process2
          -> FilePath  -- ^ output 
          -> IO () 
startMine lhefile lhefile2 lhefile3 outfile =
    withFile outfile WriteMode $ 
    -- flip ($) stdout $
      \h -> do 
        ((_,_,lst1),_) <- processFile 
                            (lheventIter $  zipStreamWithList [1..]  =$ iter h ) 
                            lhefile 
        ((_,_,lst2),_) <- processFile 
                            (lheventIter $  zipStreamWithList [1..]  =$ iter h ) 
                            lhefile2 
        ((_,_,lst3),_) <- processFile 
                            (lheventIter $ zipStreamWithList [1..] =$ iter h )
                            lhefile3 
        zipWithM3 (iw h) lst1 lst2 lst3
        hPutStrLn h $ "lst1 = " ++ show (length lst1)
        hPutStrLn h $ "lst2 = " ++ show (length lst2)
        hPutStrLn h $ "lst3 = " ++ show (length lst3)
  where iw h (Just (_,a)) (Just (_,b)) (Just (_,c)) = interwine3 h a b c
        iw _ _ _ _ = return () 
        iter h = EL.foldM (\lst a -> maybe (return lst) 
                                       (\(n,x) -> return (a:lst) ) a) 
                          []


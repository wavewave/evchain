module Main where

import System.Console.CmdArgs

import Data.List 
import qualified Data.IntMap as M
import Data.Traversable (traverse)

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
import HEP.Automation.MadGraph.EventChain.Print 


-- | 

zipWithM3 :: (Monad m) => (a -> b -> c -> m d) -> [a] -> [b] -> [c] -> m [d]
zipWithM3 f xs ys zs = sequence (zipWith3 f xs ys zs)


-- | 



testCross :: CrossID
testCross = GCross (XNode 1) incoms outgos  

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





testDecay2 :: DecayID
testDecay2 = GDecay (DNode (1,3999) 12, [ (GTerminal (TNode (1,33)))
                                        , testDecay ])


testCross2 :: CrossID
testCross2 = GCross (XNode 399) incoms [ GTerminal (TNode (1,-1))
                                       , testDecay2 ] 
                    

testCross3 = GCross (XNode 1) incoms outgos2 

testCross4 = GCross (XNode 1)
                    [ GTerminal (TNode (1,1000021)) ]
                    [ GTerminal (TNode (2,-1)) 
                    , GTerminal (TNode (3,1))
                    , testDecay ]
                    

testDecay :: DecayID 
testDecay = GDecay (DNode (4,1000022) 33, [ (GTerminal (TNode (1,-2)))
                                          , (GTerminal (TNode (2,-1)))
                                          , (GTerminal (TNode (3,-1)))
                                          , (GTerminal (TNode (4,9000202))) ])



testCross5 = GCross (XNode 1) 
                    [ GTerminal (TNode (1,21)) 
                    , GTerminal (TNode (2,21)) ] 
                    [ testDecay_go1 
                    , testDecay_go2 ] 


testDecay_go1 = GDecay (DNode (3,1000021) 2, [ GTerminal (TNode (1,-1)) 
                                             , GTerminal (TNode (2,1))
                                             , testDecay_n11 ])

testDecay_go2 = GDecay (DNode (4,1000021) 3, [ GTerminal (TNode (1,-1))
                                             , GTerminal (TNode (2,1))
                                             , testDecay_n12 ])

testDecay_n11 :: DecayID 
testDecay_n11 = GDecay (DNode (3,1000022) 4, [ (GTerminal (TNode (1,-2)))
                                             , (GTerminal (TNode (2,-1)))
                                             , (GTerminal (TNode (3,-1)))
                                             , (GTerminal (TNode (4,9000202))) ])

testDecay_n12 :: DecayID 
testDecay_n12 = GDecay (DNode (4,1000022) 5, [ (GTerminal (TNode (1,-2)))
                                             , (GTerminal (TNode (2,-1)))
                                             , (GTerminal (TNode (3,-1)))
                                             , (GTerminal (TNode (4,9000202))) ])



-- | 

main :: IO () 
main = do 
  let lhefile1 = "gogo.lhe.gz"
      lhefile2 = "godbardn1.lhe.gz" -- "test.lhe.gz"
      lhefile3 = "n1uddx.lhe.gz"
  ((_,_,lst1),_) <- processFile 
                      (lheventIter $ zipStreamWithList [1..] =$ iter )
                      lhefile1       
  ((_,_,lst2),_) <- processFile 
                      (lheventIter $ zipStreamWithList [1..] =$ iter )
                      lhefile2      
  ((_,_,lst3),_) <- processFile 
                      (lheventIter $ zipStreamWithList [1..] =$ iter )
                      lhefile3       



  let Just (_, Just (fstev,_,_)) = head lst1 
      Just (_, Just (fstev2,_,_)) = head lst2
      Just (_, Just (fstev3,_,_)) = head lst3
      LHEvent einfo pinfos = fstev
  putStrLn $ lheFormatOutput fstev
  putStrLn "=====" 

  let action x = putStrLn (show x)
  traverse action testCross5
  -- putStrLn $ show (getProcessFromCross2 testCross4) 


  let tmpmap = M.fromList [(1,fstev),(2,fstev2),(3,fstev2),(4,fstev3),(5,fstev3)]
      rmatch = matchFullCross tmpmap testCross5
  case rmatch of
    Left err-> putStrLn err
    Right fcross -> do 
      putStrLn $ "number of particles = " ++ show (countPtls fcross)
      -- putStrLn $ concatMap pformat (accumLHEvent fcross)
      putStrLn "---------------" 
      -- putStrLn $ concatMap pformat (chainDecay fcross ) 
      accumTotalEvent fcross 
      return ()
 where iter = EL.foldM (\lst a -> maybe (return lst) 
                                         (\(n,x) -> return (a:lst) ) a)
                       []

{-  let r = allFindPtlIDStatus [ (1,-1,1)
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
      putStrLn (concatMap pformat lst3) -}


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


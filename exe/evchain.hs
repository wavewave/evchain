module Main where

import System.Console.CmdArgs


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

{-
testCross :: GCross XNode DNode TNode  
testCross = GCross incoms outgos (PNode 333)

incoms = [ GTerminal (TNode 444) ]

outgos = [ GTerminal (TNode 555) 
         , GDecay (DNode 334 222, [ GTerminal (TNode 666)
                                  , GTerminal (TNode 999) ] )
         ]
-}


-- | 

main :: IO () 
main = do 
  let lhefile = "test.lhe.gz"
  -- putStrLn (printCrossStr testCross)
  -- startMine "test.lhe.gz" "test2.lhe.gz" "test3.lhe.gz"
  --           "testoutput.dat"
  ((_,_,lst1),_) <- processFile 
                      (lheventIter $ zipStreamWithList [1..] =$ iter )
                      lhefile       
  let Just (_, Just (fstev,_,_)) = head lst1 
      LHEvent einfo pinfos = fstev
  putStrLn $ lheFormatOutput fstev
  let (Just pinfo1, pinfos') = (findPtlIDStatus (1000021,2) pinfos)
  putStrLn $ pformat pinfo1
  putStrLn $ concatMap pformat pinfos'

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


{-
main :: IO () 
main = do 
  putStrLn "evchain"
  param <- cmdArgs mode

  commandLineProcess param

-}
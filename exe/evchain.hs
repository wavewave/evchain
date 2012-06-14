module Main where


import Control.Monad.State

import Data.List 
import qualified Data.IntMap as M

import HEP.Parser.LHEParser.Parser.Conduit
import HEP.Automation.EventAnalysis.FileDriver 


import HEP.Util.Count 

import HEP.Parser.LHEParser.Type 


import System.IO

import           Data.Conduit hiding (sequence)
import qualified Data.Conduit.List as CL
import           Data.Conduit.Util.Control as CU (zipStreamWithList,zipSinks3,dropWhile,takeFirstN) 
import           Data.Conduit.Util.Count


import HEP.Automation.MadGraph.EventChain.Type
import HEP.Automation.MadGraph.EventChain.LHEConn
import HEP.Automation.MadGraph.EventChain.Print 
import HEP.Automation.MadGraph.EventChain.Spec

import Prelude hiding (dropWhile)

-- | 

zipWithM3 :: (Monad m) => (a -> b -> c -> m d) -> [a] -> [b] -> [c] -> m [d]
zipWithM3 f xs ys zs = sequence (zipWith3 f xs ys zs)


-- | 

zipWithM5 :: (Monad m) => (a -> b -> c -> d -> e -> m f) 
          -> [a] -> [b] -> [c] -> [d] -> [e] -> m [f]
zipWithM5 f x1s x2s x3s x4s x5s = sequence (zipWith5 f x1s x2s x3s x4s x5s)

-- | 

ppgogo :: SCross 
ppgogo = x 1 (jet,jet,[gouun1decay1,gouun1decay2])

-- | 

gouun1decay1 :: SDecayTop
gouun1decay1 = d 2 (1000021,[2,-2,n1xueddecay1])

-- | 

gouun1decay2 :: SDecayTop
gouun1decay2 = d 3 (1000021,[2,-2,n1xueddecay2])

-- | 

n1xueddecay1 :: SDecayTop
n1xueddecay1 = d 4 (1000022,[ppair 2, ppair 11, ppair 1, ppair 9000201])

-- | 

n1xueddecay2 :: SDecayTop 
n1xueddecay2 = d 5 (1000022,[ppair 2, ppair 11, ppair 1, ppair 9000201])


-- |

workfunc5 :: LHEvent -> LHEvent -> LHEvent -> LHEvent -> LHEvent -> IO ()
workfunc5 e1 e2 e3 e4 e5 = do 
    let tmpmap = M.fromList [(1,e1),(2,e2),(3,e3),(4,e4),(5,e5)]
        rmatch = matchFullCross tmpmap (makeCrossID ppgogo)
    case rmatch of
      Left err-> do
        error err
      Right fcross -> do 
        -- putStrLn $ "number of particles = " ++ show (countPtls fcross)
        -- putStrLn $ concatMap pformat (accumLHEvent fcross)
        -- putStrLn "---------------" 
        -- putStrLn $ concatMap pformat (chainDecay fcross ) 
        pinfos <- accumTotalEvent fcross 
        putStrLn $ lheFormatOutput $ LHEvent (EvInfo 0 0 0 0 0 0) pinfos 
        return ()




-- | 

main :: IO () 
main = do 
    let lhefile1 = "gogo_qld.lhe.gz"
        lhefile2 = "gon1uu_qld_run01.lhe.gz" -- "test.lhe.gz"
        lhefile3 = "gon1uu_qld_run02.lhe.gz"
        lhefile4 = "n1uedx_qld_run01.lhe.gz"
        lhefile5 = "n1uedx_qld_run02.lhe.gz"
    lst1 <- proc lhefile1       
    lst2 <- proc lhefile2 
    lst3 <- proc lhefile3  
    lst4 <- proc lhefile4
    lst5 <- proc lhefile5

    -- let f = (workfunc3 . combinefunc3) :: Int 
    zipWithM5 workfunc5 lst1 lst2 lst3 lst4 lst5 
    return ()
  where iter = CL.foldM (\lst (n,a) -> maybe (return lst) 
                                         (\(x,_,_) -> return (x:lst)) a)

                       []
        filesrc :: Handle -> Source CountIO (Maybe (LHEvent,PtlInfoMap,[DecayTop PtlIDInfo]))
        filesrc ih = ungzipHandle ih =$= lheventIter {- =$= takeFirstN 20 -} 
        combsrc :: Handle -> Source CountIO (Int, (Maybe (LHEvent,PtlInfoMap,[DecayTop PtlIDInfo])))
        combsrc ih = zipStreamWithList [1..] (filesrc ih)
        action ih = evalStateT (combsrc ih $$ zipSinks3 countIter countMarkerIter iter) (0 :: Int)
        proc lhe = do h <- openFile lhe ReadMode
                      (_,_,lst) <- action h 
                      hClose h 
                      return lst


-- |

main' :: IO ()
main' = do 
    let lhefile1 = "pp_gogo_events.lhe.gz"
    lst1 <- proc lhefile1 
    print lst1 
  where {- iter = CL.foldM (\lst (n,a) -> maybe (return lst) 
                                         (\(x,_,_) -> return (x:lst)) a)

                       []
        filesrc :: Handle -> Source CountIO (Maybe (LHEvent,PtlInfoMap,[DecayTop PtlIDInfo]))
        filesrc ih = ungzipHandle ih =$= lheventIter
        combsrc :: Handle -> Source CountIO (Int, (Maybe (LHEvent,PtlInfoMap,[DecayTop PtlIDInfo])))
        combsrc ih = zipStreamWithList [1..] (filesrc ih)
        action ih = evalStateT (combsrc ih $$ zipSinks3 countIter countMarkerIter iter) (0 :: Int)
        proc lhe = do h <- openFile lhe ReadMode
                      (_,_,lst) <- action h 
                      hClose h 
                      return lst -}
        proc lhe = do h <- openFile lhe ReadMode 
                      lst <- ungzipHandle h =$= {- takeFirstN 100 =$= -} parseEvent $$ CL.consume
-- CU.dropWhile (not.isEventStart) $$ CL.consume -- chunkLHEvent $$ CL.consume -- =$= parseEvent $$ CL.consume 
                      hClose h 
                      return lst 


-- | 

{-

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

-}


-- | 

-- combinefunc3 (Just (_, Just (e1,_,_))) (Just (_, Just (e2,_,_))) (Just (_, Just (e3,_,_))) = (e1,e2,e3)

{-
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
-}


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

{- 

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
        iter h = CL.foldM (\lst a -> maybe (return lst) 
                                       (\(n,x) -> return (a:lst) ) a) 
                          []


-}

{-


n1xlledecay1 = GDecay (DNode (3,1000022) 4, [ GTerminal (TNode (1,-11))
                                            , GTerminal (TNode (2,11))
                                            , GTerminal (TNode (3,-14))
                                            , GTerminal (TNode (4,-9000201)) ])

n1xlledecay2 = GDecay (DNode (3,1000022) 5, [ GTerminal (TNode (1,-11))
                                            , GTerminal (TNode (2,11))
                                            , GTerminal (TNode (3,-14))
                                            , GTerminal (TNode (4,-9000201)) ])


gouun1decay1 = GDecay (DNode (3,1000021) 2, [ GTerminal (TNode (1,2))
                                            , GTerminal (TNode (2,-2))
                                            , n1xlledecay1 ] )

gouun1decay2 = GDecay (DNode (4,1000021) 3, [ GTerminal (TNode (1,2))
                                            , GTerminal (TNode (2,-2))
                                            , n1xlledecay2 ] )


ppgogo = GCross (XNode 1) 
                [ GTerminal (TNode (1,21)) 
                , GTerminal (TNode (2,21)) ] 
                [ gouun1decay1 
                , gouun1decay2 ]


-}

-- | 

{-
main :: IO ()
main  = do
  -- putStrLn "he"
  -- let x = (3  :: Spec)
  -- print x

  -- let y = X (21,21,[1000021,1000021])
  let top = x 1 (21,21,[d1,d2])
      d1 = d 2 (1000021, [2,-2,1000022])
      d2 = d 3 (1000021, [1,-1,1000022])

  print top 

  print $ makeCrossID top   
-}

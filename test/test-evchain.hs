module Main where

import Control.Monad.Identity
import Control.Monad.Error
import System.Exit (exitFailure, exitSuccess)

main :: IO ()
main = do 
    putStrLn "test-evchain:"
    (return.runIdentity.runErrorT) testPure >>= 
      either (\msg->putStrLn msg >> exitFailure) (const (return ()))
    runErrorT testIO >>= 
      either (\msg->putStrLn msg >> exitFailure) (const (return ())) 

    exitSuccess


guardMsg :: (Monad m) => String -> m Bool ->  ErrorT String m () 
guardMsg msg act = do b <- lift act
                      if b then return () else throwError msg 


testPure :: ErrorT String Identity () 
testPure = do 
    guardMsg "fail test1" (return test1) 


test1 = True

testIO :: ErrorT String IO () 
testIO = do 
    guardMsg "fail testIO1" testIO1

testIO1 :: IO Bool 
testIO1 = return False




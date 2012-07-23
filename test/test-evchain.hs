module Main where

-- from other packages from others 
import Control.Monad.Identity
import Control.Monad.Error
import System.Exit (exitFailure, exitSuccess)
-- from this package
import Test.EventChain.Common
import Test.EventChain.LHEParse
import Test.EventChain.Match
import Test.EventChain.Spec


main :: IO ()
main = do 
    putStrLn "test-evchain:"
    (return.runIdentity.runErrorT) testPure >>= 
      either (\msg->putStrLn msg >> exitFailure) (const (return ()))
    runErrorT testIO >>= 
      either (\msg->putStrLn msg >> exitFailure) (const (return ()))
    exitSuccess



-- | 

testPure :: ErrorT String Identity () 
testPure = do 
    guardMsg "fail spec1x_test" (return spec1x_test) 
    guardMsg "fail spec2x_test" (return spec2x_test)


-- | 

testIO :: ErrorT String IO () 
testIO = do 
    guardMsg "fail test_parse_unzip" test_parse_unzip
    guardMsg "fail test_parse_zip" test_parse_zip
    test_match_driver

  





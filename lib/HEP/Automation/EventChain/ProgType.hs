{-# LANGUAGE DeriveDataTypeable #-}

module HEP.Automation.MadGraph.EventChain.ProgType where 

import System.Console.CmdArgs

data Evchain = Test 
              deriving (Show,Data,Typeable)

test :: Evchain
test = Test 

mode = modes [test]


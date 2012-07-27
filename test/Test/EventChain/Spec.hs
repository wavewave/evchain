module Test.EventChain.Spec where 

import Data.Foldable
import Data.Traversable
-- 
import HEP.Automation.EventChain.SpecDSL
import HEP.Automation.EventChain.Type.Skeleton
import HEP.Automation.EventChain.Type.Spec
--
import Prelude hiding (foldr)


{-
-- unit test 1 
crosstest :: Cross String (Int,String) Int 
crosstest = MkC "cross" ( MkT 11, MkT 12 ) [ test , test5 ] 

test5 :: Decay (Int,String) Int 
test5 = MkD (5,"test5") [ MkT 99] 


test :: Decay (Int,String) Int
test = MkD (1,"test") [ test2, test3 ] 

test2 :: Decay (Int,String) Int
test2 = MkD (2,"test2") [ MkT 1, MkT 2, MkT 3 ] 
 
test3 :: Decay (Int,String) Int 
test3 = MkD (3,"test3") [ test4 ]   

test4 :: Decay (Int,String) Int 
test4 = MkD (4,"test4") [ MkT 4 ] 


ntest = DecayF test

ncross = CrossF crosstest 

main1 = do 
  traverse putStrLn ncross 
  putStrLn $ foldr (++) "" ncross
-}


-- unit test 2 

spec1x_explicit :: DCross
spec1x_explicit = MkC ()  (MkT [1], MkT [1,2] )
                          [ spec1d1_explicit, MkT [3] ] 

spec1d1_explicit :: DDecay 
spec1d1_explicit = MkD [3] 
                       [ MkT [4,5,6]
                       , MkT [4] ]

spec1x_implicit :: DCross
spec1x_implicit = x (1,t [1,2], [ spec1d1_implicit, 3] )

spec1d1_implicit :: DDecay
spec1d1_implicit = d ([3],[t [4,5,6], 4] )


spec1x_test :: Bool 
spec1x_test = spec1x_explicit == spec1x_implicit 


-- | Spec test 2

spec2x_explicit = MkC "proc1" (MkT [1], MkT [2]) [spec2d1_explicit, MkT [4]]

spec2d1_explicit = MkD [("proc2_1",2), ("proc2_2",-2)] [MkT [4], MkT [5], MkT [6]]



spec2x_implicit = xp "proc1" (1,2,[spec2d1_implicit,4])

spec2d1_implicit = dp ["proc2_1","proc2_2"] (ppair 2,[4,5,6])

spec2x_test :: Bool 
spec2x_test = spec2x_explicit == spec2x_implicit 

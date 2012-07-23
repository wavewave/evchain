module Test.EventChain.LHEParse where

-- from other packages from others
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.Maybe (catMaybes)
import           System.Directory
import           System.FilePath
import           System.IO
-- from this package
import HEP.Automation.EventChain.FileDriver
--
import Paths_evchain 



-- | 

test_parse_unzip :: IO Bool 
test_parse_unzip = do 
  dir <- getDataDir 
  let fn = dir </> "test" </> "resources" </> "pp_gogo_events.lhe"
  h <- openFile fn ReadMode
  lst <- evtsHandle False h $$ CL.consume
  return $ (length (catMaybes lst)) == 2

-- | 
  
test_parse_zip :: IO Bool 
test_parse_zip = do 
  dir <- getDataDir 
  let fn = dir </> "test" </> "resources" </> "pp_gogo_events.lhe.gz"
  h <- openFile fn ReadMode
  lst <- evtsHandle True h $$ CL.consume
  return $ (length (catMaybes lst)) == 2


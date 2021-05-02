module Ch11
    ( ch11
    ) where

import qualified Data.ByteString as BS
import System.Random (genByteString, mkStdGen, getStdGen, StdGen)

ch11 = do
  ran <- getStdGen
  print $ randomAESKey ran

randomAESKey :: StdGen -> BS.ByteString
randomAESKey ran = fst $ genByteString 16 $ ran

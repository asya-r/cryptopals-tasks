module Ch6
  ( hammingDistance
  ) where

import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString as BS
import Data.Bits (popCount, xor)

hammingDistance :: String -> String -> Int
hammingDistance str1 str2 =
  let xored = BS.zipWith xor (BS8.pack str1) (BS8.pack str2)
  in sum $ map popCount xored

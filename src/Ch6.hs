module Ch6
  ( ch6
  , findKey
  ) where

import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString as BS
import Data.Bits (popCount, xor)
import qualified Data.ByteString.Base64 as Base64
import GHC.Float (int2Float)
import Data.List.Split (chunksOf)
import Data.List (transpose)
import System.IO.Unsafe (unsafePerformIO)

import Utils (slices, sortTupleListByFst)
import Ch3 (findByteKey)
import Ch5 (repeatingKeyXOR)
import Ch1 (hexToBase64)

ch6 :: BS.ByteString
ch6 = findKey $ BS8.pack $ unsafePerformIO $ readFile "files/6.txt"

hammingDistance :: BS.ByteString -> BS.ByteString -> Int
hammingDistance str1 str2 =
  let xored = BS.zipWith xor str1 str2
  in sum $ map popCount xored

findKeysize :: BS.ByteString -> Int
findKeysize bs =
  let keysizeRes keysize bs = minimum $ map ((/ int2Float keysize) . (\x -> int2Float $ hammingDistance (fst x) (snd x))) (slices keysize 3 bs) -- TODO: 3->length-dependent var
      results = map (\x -> (keysizeRes x bs, x)) [2..40]
      bestKeysize = snd $ head (sortTupleListByFst results)
  in bestKeysize

findKey :: BS.ByteString -> BS.ByteString
findKey b64Str =
  let bs = Base64.decodeLenient b64Str
      keysize = findKeysize bs
      blocks = transpose $ chunksOf keysize (BS.unpack bs)
      blockKeys = map (fst . findByteKey . BS.pack) blocks
  in BS.pack blockKeys -- repeatingKeyXOR bs (BS.pack blockKeys)

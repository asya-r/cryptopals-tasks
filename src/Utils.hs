module Utils
  ( sortTupleListByFst
  , slices
  , from16
  , decodeB64
  , hexToBase64
  , xorBS
  , countDupBlocks
  , blocks
  , countDuplicates
  ) where

import Data.List (sortBy, nub)
import qualified Data.ByteString as BS
import Data.ByteString.Base16 (encode, decode)
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as BS8
import Data.Bits (xor)

sortTupleListByFst :: (Ord a, Ord b) => [(a, b)] -> [(a, b)]
sortTupleListByFst = sortBy (\(a, _) (b, _) -> compare a b)

-- unsafe!
slices :: Int -> Int -> BS.ByteString -> [(BS.ByteString, BS.ByteString)]
slices keysize pairNum bs = hlp keysize pairNum bs [] where
  hlp _ 0 _ list = list
  hlp keysize left bs list = hlp keysize (left - 1) (BS.drop (keysize * 2) bs) (list ++ [(BS.take keysize bs, BS.take keysize $ BS.drop keysize bs)])

from16 :: BS.ByteString -> BS.ByteString
from16 = fst . decode

decodeB64 :: String -> BS.ByteString
decodeB64 text = case decoded text of
  Left err -> error err
  Right bs -> bs
  where
    decoded = Base64.decode . BS8.filter (/= '\n') . BS8.pack

hexToBase64 :: BS.ByteString -> BS.ByteString
hexToBase64 = Base64.encode . from16

xorBS :: BS.ByteString -> BS.ByteString -> BS.ByteString
xorBS str1 str2 = BS.pack $ BS.zipWith xor str1 str2

countDupBlocks :: BS.ByteString -> Int
countDupBlocks = countDuplicates . (blocks 16)

blocks :: Int -> BS.ByteString -> [BS.ByteString]
blocks blockSize bs = hlp bs [] where
  hlp bs bls = if BS.length bs < blockSize
               then bls
               else hlp (BS.drop blockSize bs) (bls ++ [BS.take blockSize bs])

countDuplicates :: (Ord a) => [a] -> Int
countDuplicates xs = (length xs) - (length (nub xs))

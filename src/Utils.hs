module Utils
  ( sortTupleListByFst
  , slices
  , from16
  , decodeB64
  ) where

import Data.List (sortBy)
import qualified Data.ByteString as BS
import Data.ByteString.Base16 (encode, decode)
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as BS8

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

module Utils
  ( sortTupleListByFst
  , slices
  , from16
  ) where

import Data.List (sortBy)
import qualified Data.ByteString as BS
import Data.ByteString.Base16 (encode, decode)

sortTupleListByFst :: (Ord a, Ord b) => [(a, b)] -> [(a, b)]
sortTupleListByFst = sortBy (\(a, _) (b, _) -> compare a b)

-- unsafe!
slices :: Int -> Int -> BS.ByteString -> [(BS.ByteString, BS.ByteString)]
slices keysize pairNum bs = hlp keysize pairNum bs [] where
  hlp _ 0 _ list = list
  hlp keysize left bs list = hlp keysize (left - 1) (BS.drop (keysize * 2) bs) (list ++ [(BS.take keysize bs, BS.take keysize $ BS.drop keysize bs)])

from16 :: BS.ByteString -> BS.ByteString
from16 = fst . decode

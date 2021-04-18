module Ch3
  ( findByteKey
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import Data.Bits (xor)
import Data.ByteString.Base16 (encode, decode)
import Data.Word (Word8)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import GHC.Float (int2Float)
import Data.List (sortBy)
import Data.Char (toLower)

findByteKey :: BS.ByteString -> (Word8, BS.ByteString)
findByteKey str =
  let (bestScore, itsChar) = last $ sortBy (\(a, _) (b, _) -> compare a b) (allBytesResults str)
  in (itsChar, xorBS_Byte str itsChar)

xorBS_Byte :: BS.ByteString -> Word8 -> BS.ByteString
xorBS_Byte str byte = BS.pack $ map (xor byte) (BS.unpack $ from16 str) where
  from16 = fst . decode

allBytesResults :: BS.ByteString -> [(Float, Word8)]
allBytesResults str = map (\byte -> ((res str byte), byte)) [0..255] where
  res s = scoreStrNaivImpl . C.unpack . (xorBS_Byte s)

scoreStrNaivImpl :: String -> Float
scoreStrNaivImpl str = (sum (map freq str)) / (int2Float $ length str) where
  freq c = fromMaybe 0.00 $ Map.lookup (toLower c) (Map.fromList letterFrequency)

letterFrequency :: [(Char, Float)]
letterFrequency =
  [ ('e', 12.02), ('t', 9.10), ('a', 8.12)
  , ('o', 7.68), ('i', 7.31), ('n', 6.95)
  , ('s', 6.28), ('r', 6.02), ('h', 5.92)
  , ('d', 4.32), ('l', 3.98), ('u', 2.88)
  , ('c', 2.71), ('m', 2.61), ('f', 2.30)
  , ('y', 2.11), ('w', 2.09), ('g', 2.03)
  , ('p', 1.82), ('b', 1.49), ('v', 1.11)
  , ('k', 0.69), ('x', 0.17), ('q', 0.11)
  , ('j', 0.10), ('z', 0.07), (' ', 5.00) ] -- space is a workaround to improve scoring

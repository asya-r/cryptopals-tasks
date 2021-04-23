{-# LANGUAGE OverloadedStrings #-}
module Ch3
  ( ch3
  , findByteKey
  , scoreStrNaivImpl
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Bits (xor)
import Data.Word (Word8)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import GHC.Float (int2Float)
import Data.Char (toLower)

import Utils (sortTupleListByFst, from16)

ch3 :: (Word8, BS.ByteString)
ch3 = findByteKey $ from16 "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"

findByteKey :: BS.ByteString -> (Word8, BS.ByteString)
findByteKey str =
  let (bestScore, itsKey) = last $ sortTupleListByFst $ allBytesResults str
      decodedString = xorBS_Byte str itsKey
  in (itsKey, decodedString)

xorBS_Byte :: BS.ByteString -> Word8 -> BS.ByteString
xorBS_Byte str byte = BS.pack $ map (xor byte) (BS.unpack str)

allBytesResults :: BS.ByteString -> [(Float, Word8)]
allBytesResults str = map (\byte -> ((res str byte), byte)) [0..255] where
  res s = scoreStrNaivImpl . BS8.unpack . (xorBS_Byte s)

scoreStrNaivImpl :: String -> Float
scoreStrNaivImpl str = (sum (map freq str)) / (int2Float $ length str) where
  freq c = fromMaybe 0.00 $ Map.lookup (toLower c) letterFrequency

letterFrequency :: Map.Map Char Float
letterFrequency = Map.fromList
  [ ('e', 12.02), ('t', 9.10), ('a', 8.12)
  , ('o', 7.68), ('i', 7.31), ('n', 6.95)
  , ('s', 6.28), ('r', 6.02), ('h', 5.92)
  , ('d', 4.32), ('l', 3.98), ('u', 2.88)
  , ('c', 2.71), ('m', 2.61), ('f', 2.30)
  , ('y', 2.11), ('w', 2.09), ('g', 2.03)
  , ('p', 1.82), ('b', 1.49), ('v', 1.11)
  , ('k', 0.69), ('x', 0.17), ('q', 0.11)
  , ('j', 0.10), ('z', 0.07), (' ', 4.00) ] -- space is a workaround to improve scoring

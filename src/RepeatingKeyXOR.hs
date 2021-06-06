module RepeatingKeyXOR
  ( findKey
  , repeatingKeyXOR
  ) where

import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString as BS
import Data.Bits (popCount, xor)
import qualified Data.ByteString.Base64 as Base64
import GHC.Float (int2Float)
import Data.List.Split (chunksOf)
import Data.List (transpose, sortBy)

import Utils (slices, sortTupleListByFst, xorBS)
import SingleByteXORAttack (findByteKey, scoreStrNaivImpl)

repeatingKeyXOR :: BS.ByteString -> BS.ByteString -> BS.ByteString
repeatingKeyXOR str key =
  let str'    = BS.unpack str
      key'    = take (length str') (cycle $ BS.unpack key)
  in xorBS str (BS.pack key')

hammingDistance :: BS.ByteString -> BS.ByteString -> Int
hammingDistance str1 str2 =
  let xored = BS.zipWith xor str1 str2
  in sum $ map popCount xored

findKeysizes :: BS.ByteString -> [Int]
findKeysizes bs =
  let keysizeRes keysize bs = maximum $ map ((/ int2Float keysize) . (\x -> int2Float $ hammingDistance (fst x) (snd x))) (slices keysize 3 bs) -- TODO: 3->length-dependent var
      results = map (\x -> (keysizeRes x bs, x)) [2..40]
      bestKeysizes = snd <$> take 5 (sortTupleListByFst results)
  in bestKeysizes

findKey :: BS.ByteString -> String
findKey b64Str =
  let bs = Base64.decodeLenient b64Str
      keysizes = findKeysizes bs
      keys = map (\x -> keyFromKeysize x bs) keysizes
      decrypted = map (\x -> BS8.unpack $ repeatingKeyXOR bs x) keys
      bestFromDecrypted = last $ sortBy (\a b -> compare (scoreStrNaivImpl a) (scoreStrNaivImpl b)) decrypted
  in bestFromDecrypted

keyFromKeysize :: Int -> BS.ByteString -> BS.ByteString
keyFromKeysize keysize uncrypted =
  let blocks = transpose $ chunksOf keysize (BS.unpack uncrypted)
      blockKeys = map (fst . findByteKey . BS.pack) blocks
  in BS.pack blockKeys

{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Ch4
  ( ch4
  , findEncryptedString
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Word (Word8)
import System.IO.Unsafe (unsafePerformIO)

import Ch3 (findByteKey, scoreStrNaivImpl)
import Utils (sortTupleListByFst, from16)

ch4 :: String
ch4 = findEncryptedString $ encode <$> (unsafePerformIO $ lines <$> readFile "files/4.txt") where
  encode = from16 <$> BS8.pack

findEncryptedString :: [BS.ByteString] -> String
findEncryptedString strings =
  let allBest = map (findByteKey) strings -- [(key, decodedStr)]
      changeKeyToScore = (\bs -> (scoreStrNaivImpl bs, bs)) . BS8.unpack . snd -- [(keyScore, decodedStr)]
      sortedBest = sortTupleListByFst (map changeKeyToScore allBest)
  in snd $ last $ sortedBest

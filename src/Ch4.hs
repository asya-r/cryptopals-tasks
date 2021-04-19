{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Ch4
  ( findEncryptedString
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Word (Word8)

import Ch3 (findByteKey, scoreStrNaivImpl)
import Utils (sortTupleListByFst)

findEncryptedString :: String -> IO String
findEncryptedString fileName = do
  strings <- lines <$> readFile fileName
  let allBest = map (findByteKey . BS8.pack) strings
      changeKeyToScore = (\bs -> (scoreStrNaivImpl bs, bs)) . BS8.unpack . snd
      sortedBest = sortTupleListByFst (map changeKeyToScore allBest)
  return $ snd $ last $ sortedBest

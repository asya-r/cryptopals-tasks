{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Ch4
  ( findEncryptedString
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import Data.Word (Word8)

import Ch3 (findByteKey, scoreStrNaivImpl)
import Utils (sortTupleListByFst)

findEncryptedString :: String -> IO String
findEncryptedString fileName = do
  strings <- lines <$> readFile fileName
  let allBest = map (findByteKey . C.pack) strings
      changeKeyToScore = (\bs -> (scoreStrNaivImpl bs, bs)) . C.unpack . snd
      sortedBest = sortTupleListByFst (map changeKeyToScore allBest)
  return $ snd $ last $ sortedBest

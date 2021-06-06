{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module RandomStuff
    ( checkOracle
    , detectEncryption
    , generateKey
    , generateKeyPrefix
    , generateIvKey
    ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import System.Random (genByteString, getStdGen, random, randomR)

import AES (encryptAES_ECB, encryptAES_CBC)
import Utils (countDupBlocks)

checkOracle :: IO (String, String)
checkOracle = do
  ran1 <- getStdGen
  plaintext <- BS8.pack <$> readFile "files/testOracle.txt"
  let (mode::Bool, ran2) = random ran1
      (key, ran3) = genByteString 16 ran2
      (beforeLength, ran4) = randomR (5, 10) ran3
      (before, ran5) = genByteString beforeLength ran4
      (afterLength, ran6) = randomR (5, 10) ran5
      (after, ran7) = genByteString afterLength ran6
      (iv, ran8) = genByteString 16 ran7
      encrypted = if mode
                  then encryptAES_CBC iv key (BS.concat [before, plaintext, after])
                  else encryptAES_ECB key (BS.concat [before, plaintext, after])
      detected = detectEncryption encrypted
  return (detected, if mode then "CBC" else "ECB")

detectEncryption :: BS.ByteString -> String
detectEncryption bs = if (countDupBlocks bs) > 0
                      then "ECB"
                      else "CBC"

generateKey :: IO BS.ByteString
generateKey = do
  ran <- getStdGen
  let (key, _) = genByteString 16 ran
  return key

generateKeyPrefix :: IO (BS.ByteString, BS.ByteString)
generateKeyPrefix = do
  ran <- getStdGen
  let (key, ran1) = genByteString 16 ran
      (prefixLength, ran2) = randomR (0, 99) ran1
      (prefix, _) = genByteString prefixLength ran2
  return (key, prefix)

generateIvKey :: IO (BS.ByteString, BS.ByteString)
generateIvKey = do
  ran <- getStdGen
  let (key, ran1) = genByteString 16 ran
      (iv, _) = genByteString 16 ran1
  return (iv, key)

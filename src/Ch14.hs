{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Ch14
    ( ch14
    ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8

import Ch11 (generateKeyPrefix)
import Ch7 (encryptAES_ECB)
import Ch12 (ecbDecrypt)
import Utils (decodeB64)

ch14 :: IO ()
ch14 = do
  encodedPlaintext <- readFile "files/12.txt"
  (key, prefix) <- generateKeyPrefix
  let plaintext = decodeB64 encodedPlaintext
      oracleFunction (bs::BS.ByteString) = encryptAES_ECB key $ BS8.concat [prefix, bs, plaintext]
      decrypted = ecbDecrypt oracleFunction
  putStrLn decrypted

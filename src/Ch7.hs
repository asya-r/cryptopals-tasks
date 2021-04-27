{-# LANGUAGE OverloadedStrings #-}
module Ch7
  ( ch7
  , decryptAES_ECB
  ) where

import Crypto.Cipher.AES128 (unEcb, buildKey, AESKey128)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString.Base64 as Base64

ch7 :: String
ch7 = BS8.unpack $ decryptAES_ECB key encrypted where
  key = "YELLOW SUBMARINE"
  clean = Base64.decode . BS8.filter (/= '\n') . BS8.pack
  encrypted = case clean $ unsafePerformIO $ readFile "files/7.txt" of
    Left err -> BS8.pack err
    Right bs -> bs

decryptAES_ECB :: BS.ByteString -> BS.ByteString -> BS.ByteString
decryptAES_ECB key encrypted = case (buildKey key :: Maybe AESKey128) of
  Nothing -> error "key size mismatch"
  Just aes128key -> unEcb aes128key encrypted

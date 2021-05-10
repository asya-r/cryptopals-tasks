{-# LANGUAGE OverloadedStrings #-}
module Ch7
  ( ch7
  , encryptAES_ECB
  , encryptAES_CBC
  , decryptAES_ECB
  , decryptAES_CBC
  ) where

import Crypto.Cipher.AES128 (unEcb, buildKey, AESKey128, encryptBlock, decryptBlock)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString.Base64 as Base64
import Data.List.Split (chunksOf)

import Ch9 (pkcs7)
import Ch2 (xorBS)
import Utils (decodeB64)

ch7 :: String
ch7 = BS8.unpack $ decryptAES_ECB key encrypted where
  key = "YELLOW SUBMARINE"
  encrypted = decodeB64 $ unsafePerformIO $ readFile "files/7.txt"

encryptAES_ECB :: BS.ByteString -> BS.ByteString -> BS.ByteString
encryptAES_ECB key text = encryptAES (encECB $ buildKey' key) text

encryptAES_CBC :: BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString
encryptAES_CBC iv key text = encryptAES (encCBC iv $ buildKey' key) text

decryptAES_ECB :: BS.ByteString -> BS.ByteString -> BS.ByteString
decryptAES_ECB key encrypted = decryptAES (decECB $ buildKey' key) encrypted

decryptAES_CBC :: BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString
decryptAES_CBC iv key encrypted = decryptAES (decCBC iv $ buildKey' key) encrypted

encryptAES :: ([BS.ByteString] -> [BS.ByteString]) -> BS.ByteString -> BS.ByteString
encryptAES encMode text =
  let paddedText = pkcs7 text 16
      blocks = BS.pack <$> (chunksOf 16 (BS.unpack paddedText))
      encryptedBlocks = encMode blocks
  in BS.concat encryptedBlocks

encECB :: AESKey128 -> [BS.ByteString] -> [BS.ByteString]
encECB key blocks = encryptBlock key <$> blocks

encCBC :: BS.ByteString -> AESKey128 -> [BS.ByteString] -> [BS.ByteString]
encCBC iv key blocks = f blocks [] iv key where
  f [] enc _ _       = enc
  f (x:xs) [] iv key = f xs [encryptBlock key (xorBS x iv)] iv key
  f (x:xs) enc _ key = f xs (enc ++ [encryptBlock key (xorBS x (last enc))]) iv key

decryptAES :: ([BS.ByteString] -> [BS.ByteString]) -> BS.ByteString -> BS.ByteString
decryptAES decMode encrypted =
  let blocks = BS.pack <$> (chunksOf 16 (BS.unpack encrypted))
      decryptedBlocks = decMode blocks
  in BS.concat decryptedBlocks

decECB :: AESKey128 -> [BS.ByteString] -> [BS.ByteString]
decECB key blocks = decryptBlock key <$> blocks

decCBC :: BS.ByteString -> AESKey128 -> [BS.ByteString] -> [BS.ByteString]
decCBC iv key blocks = f blocks [] iv key where
  f [] dec _ _       = dec
  f (x:xs) [] xorWith key = f xs [xorBS xorWith (decryptBlock key x)] x key
  f (x:xs) dec xorWith key = f xs (dec ++ [xorBS xorWith (decryptBlock key x)]) x key

buildKey' :: BS.ByteString -> AESKey128
buildKey' key = case (buildKey key :: Maybe AESKey128) of
  Nothing -> error "key size mismatch"
  Just aes128key -> aes128key

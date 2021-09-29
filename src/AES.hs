{-# LANGUAGE OverloadedStrings #-}
module AES
  ( encryptAES_ECB
  , encryptAES_CBC
  , decryptAES_ECB
  , decryptAES_CBC
  , findAES_ECB
  ) where

import Crypto.Cipher.AES128 (buildKey, AESKey128, encryptBlock, decryptBlock)
import Data.Bits (xor)
import qualified Data.ByteString as BS
import Data.List.Split (chunksOf)
import Data.List (sortBy)
import Sound.OSC.Coding.Byte (encode_u16)
import Util (unzipWith)

import PKCS7 (pkcs7)
import Utils (countDuplicates, blocks, xorBS)

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

findAES_ECB :: [BS.ByteString] -> BS.ByteString
findAES_ECB bss = last $ sortBy (\x y -> compare (count x) (count y)) bss where
  count = countDuplicates . (blocks 16)

cryptAES_CTR :: AESKey128 -> Int -> BS.ByteString -> BS.ByteString
cryptAES_CTR key nonce text = BS.pack $ unzipWith xor $ zip (BS.unpack text) (BS.unpack infEncCounter) where
  infEncCounter = BS.concat $ map (\x -> encryptBlock key (encode_u16 x)) (iterate (+1) nonce)

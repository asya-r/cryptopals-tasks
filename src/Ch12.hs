{-# LANGUAGE OverloadedStrings #-}
module Ch12
    ( ch12
    ) where

import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Base64 as Base64
import Data.Word (Word8)

import Ch11 (detectEncryption, generateKey)
import Ch9 (pkcs7)
import Ch7 (encryptAES_ECB)
import Utils (decodeB64)

ch12 :: IO ()
ch12 = do
  encodedPlaintext <- readFile "files/12.txt"
  forOracle <- BS8.pack <$> readFile "files/testOracle.txt"
  key <- generateKey
  let plaintext = BS.concat [decodeB64 encodedPlaintext, forOracle]
      decrypted = ecbDecrypt (encryptAES_ECB key) plaintext
  putStrLn $ BS8.unpack decrypted

ecbDecrypt :: (BS.ByteString -> BS.ByteString) -> BS.ByteString -> BS.ByteString
ecbDecrypt encFun bs =
  let encrypted = encFun bs
      blockSize = discoverBlockSize encFun
      isEcb = (detectEncryption encrypted) == "ECB"
      encryptSym s = encFun $ BS8.concat [(BS8.pack $ replicate (blockSize - 1) 'A'), (BS.pack [s])]
      codebook = Map.fromList $ map (\s -> (encryptSym s, s)) [0..255]
      codebookUnknown = map encryptSym (BS.unpack bs)
      decrypt = map (\u -> case Map.lookup u codebook of
                             Nothing -> error "Unknown byte"
                             Just c -> c)
                    codebookUnknown
  in if isEcb then (BS.pack decrypt) else error "It isn't ECB mode"

discoverBlockSize :: (BS.ByteString -> BS.ByteString) -> Int
discoverBlockSize encFun = hlp 0 1 where
  hlp prevSize cur =
    let curSize = BS.length $ encFun $ BS.replicate cur 0
    in if (curSize > prevSize) && (prevSize /= 0) then curSize else hlp curSize (cur + 1)

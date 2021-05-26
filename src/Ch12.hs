{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Ch12
    ( ch12
    , decryptByteAByte
    , ecbDecrypt
    ) where

import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString.Internal (w2c)
import System.Random (genByteString, getStdGen)
import Data.List (elemIndices)
import Data.List.Unique (repeated)

import Ch11 (detectEncryption, generateKey)
import Ch9 (pkcs7)
import Ch7 (encryptAES_ECB)
import Ch8 (countDupBlocks, blocks)
import Utils (decodeB64)

ch12 :: IO ()
ch12 = do
  encodedPlaintext <- readFile "files/12.txt"
  key <- generateKey
  let plaintext = decodeB64 encodedPlaintext
      oracleFunction (bs::BS.ByteString) = encryptAES_ECB key $ BS8.concat [bs, plaintext]
      decrypted = ecbDecrypt oracleFunction
  putStrLn decrypted

ecbDecrypt :: (BS.ByteString -> BS.ByteString) -> String
ecbDecrypt encFun =
  let blockSize = discoverBlockSize encFun
      isEcb = (detectEncryption $ encFun $ BS.replicate (blockSize * 3) 0) == "ECB"
  in if isEcb
     then (decryptByteAByte encFun blockSize (blockTargetOnly encFun blockSize))
     else error $ "It isn't ECB mode " ++ (show blockSize)

discoverBlockSize :: (BS.ByteString -> BS.ByteString) -> Int
discoverBlockSize encFun = hlp (BS.length $ encFun $ BS.replicate 1 0) 1 where
  hlp prevSize len =
    let lenNew = len + 1
        curSize = BS.length $ encFun $ BS.replicate lenNew 0
    in if (curSize > prevSize) && (prevSize /= 0) then curSize - prevSize else hlp curSize lenNew

decryptByteAByte' :: (BS.ByteString -> BS.ByteString) -> Int -> String
decryptByteAByte' encFun blockSize = hlp encFun "" blockSize 1 (encSize encFun) where
  encSize encFun = BS.length $ encFun ""
  hlp encFun decrypted blockSize curByte encSize
    | encSize == curByte + 1 = decrypted
    | otherwise =
       let myStringFirstAs = BS8.pack $ replicate (blockSize - (mod curByte blockSize)) 'A'
           myString = BS8.concat [myStringFirstAs, BS8.pack decrypted]
           encryptSym s = BS.take ((div curByte blockSize + 1) * 16) $ encFun $ BS8.concat [myString, (BS.pack [s])]
           codebook = Map.fromList $ map (\s -> (encryptSym s, s)) [0..255]
           unknown = BS.take ((div curByte blockSize + 1) * 16) (encFun myStringFirstAs)
       in case Map.lookup unknown codebook of
                              Nothing -> decrypted
                              Just c -> hlp encFun (decrypted ++ [w2c c]) blockSize (curByte + 1) encSize

blockTargetOnly :: (BS.ByteString -> BS.ByteString) -> Int -> (Int, Int) -- index of first target block, num of bytes
blockTargetOnly encFun blockSize = hlp encFun blockSize 0 where
         hlp encFun blockSize numAttackerBytes =
           let num = if numAttackerBytes == 0 then blockSize * 2 else numAttackerBytes + 1
               res = encFun $ BS.replicate num 0
               equalBlocks = repeated (blocks res)
           in case length equalBlocks of
             0 -> hlp encFun blockSize num
             1 -> ((maximum $ elemIndices (head equalBlocks) (blocks res)) - 1, mod num blockSize)
             otherwise -> error "there were equal blocks before"

decryptByteAByte :: (BS.ByteString -> BS.ByteString) -> Int -> (Int, Int) -> String
decryptByteAByte encFun blockSize padded = hlp encFun "" blockSize 1 (encSize encFun) padded where
  encSize encFun = BS.length $ encFun ""
  hlp encFun decrypted blockSize curByte encSize padded
    | encSize == curByte - 1 = decrypted
    | otherwise =
       let myStringFirstAs = BS8.pack $ replicate (blockSize - (mod curByte blockSize)) 'A'
           forPrefix = BS8.pack $ replicate (snd padded) 'A'
           myString = BS8.concat [myStringFirstAs, BS8.pack decrypted]
           bytesToTake = (div curByte blockSize + 1) * blockSize
           bytesToDrop = (fst padded) * blockSize
           withSym = \x -> encFun $ BS8.concat [forPrefix, myString, (BS.pack [x])]
           encryptSym s = BS.take bytesToTake (BS.drop bytesToDrop $ withSym s)
           codebook = Map.fromList $ map (\s -> (encryptSym s, s)) [0..255]
           withUnknown = encFun $ BS8.concat [forPrefix, myStringFirstAs]
           unknown = BS.take bytesToTake (BS.drop bytesToDrop withUnknown)
       in case Map.lookup unknown codebook of
                              Nothing -> decrypted
                              Just c -> hlp encFun (decrypted ++ [w2c c]) blockSize (curByte + 1) encSize padded

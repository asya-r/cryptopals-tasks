{-# LANGUAGE OverloadedStrings #-}
module Ch17
    ( ch17
    ) where

import qualified Data.ByteString as BS
import System.Random (randomR, getStdRandom)
import Data.Bits (xor)
import Sound.OSC.Coding.Convert (int_to_word8)
import Data.List (inits)

import Ch7 (decryptAES_CBC, encryptAES_CBC)
import Ch15 (stripPadding)
import Ch11 (generateIvKey)
import qualified Ch8 as Ch8
import Utils (decodeB64)

ch17 :: IO ()
ch17 = do
  (iv, key) <- generateIvKey
  strings <- lines <$> readFile "files/17.txt"
  lineNum <- getStdRandom (randomR (0,(length strings) - 1))
  let encrypted = serverEncrypt iv key (decodeB64 $ strings !! lineNum)
  print $ paddingOracleAttack encrypted (serverCheckPadding iv key)

serverEncrypt :: BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString
serverEncrypt iv key b64 = encryptAES_CBC iv key b64

serverCheckPadding :: BS.ByteString -> BS.ByteString -> BS.ByteString -> Bool
serverCheckPadding iv key encrypted =
  let decrypted = decryptAES_CBC iv key encrypted
  in case stripPadding decrypted of
    Nothing -> False
    Just _ -> True

paddingOracleAttack :: BS.ByteString -> (BS.ByteString -> Bool) -> BS.ByteString
paddingOracleAttack encrypted checkFun =
  let blockSize = 16
      blocks = Ch8.blocks blockSize encrypted
      decryptLastBlock encrCropped decrypted =
        if (BS.length decrypted == blockSize) || (BS.length encrCropped <= blockSize)
        then decrypted
        else
          let numInBlock = blockSize - (BS.length decrypted)
              curPaddingNum = BS.length decrypted + 1
              numOfByteAffected = BS.length encrCropped - (blockSize * 2) + numInBlock
              funToChangePrev (encByte, decByte) = foldl1 xor [int_to_word8 curPaddingNum, encByte, decByte]
              changedPreviousDecrypted = BS.reverse $ BS.pack $ map funToChangePrev $ BS.zip (BS.reverse $ BS.take blockSize (BS.drop (BS.length encrCropped - (blockSize * 2)) encrCropped))
                                                                                             (BS.reverse decrypted)
              allVariants = [ (x, BS.concat [ (BS.take (numOfByteAffected - 1) encrCropped)
                                           , BS.pack [x]
                                           , changedPreviousDecrypted
                                           , BS.take blockSize (BS.drop (BS.length encrCropped - blockSize) encrCropped) ])
                                       | x <- [0..255] ]
              cForTrue = filter (\x -> checkFun $ snd x) allVariants
              c' = case length cForTrue of
                1 -> fst $ head cForTrue
                2 -> fst $ head $ filter (\x -> fst x /= BS.index encrCropped (numOfByteAffected - 1)) cForTrue -- for 0x01 padding
                x -> error "check padding check function"
              c = BS.index encrCropped (numOfByteAffected - 1)
              p = foldl1 xor [c', c, int_to_word8 curPaddingNum]
          in decryptLastBlock encrCropped $ BS.concat [BS.pack [p], decrypted]
  in BS.concat $ map ((flip decryptLastBlock "") . BS.concat) (inits blocks)

{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Ch17
    ( ch17
    ) where

import qualified Data.ByteString as BS
import System.Random (randomR, getStdRandom)
import qualified Data.ByteString.Char8 as BS8

import Ch7 (decryptAES_CBC, encryptAES_CBC)
import Ch15 (stripPadding)
import Ch11 (generateIvKey)
import qualified Ch8 as Ch8
import Ch2 (xorBS)

ch17 :: IO ()
ch17 = do
  (iv, key) <- generateIvKey
  strings <- lines <$> readFile "files/17.txt"
  lineNum <- getStdRandom (randomR (0,(length strings) - 1))
  let encrypted = serverEncrypt iv key (BS8.pack $ strings !! lineNum)
  print $ serverCheckPadding iv key encrypted --(serverCheckPadding iv key)

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
      decryptLastBlock encrCropped decrypted csForTrue =
        if BS.length decrypted == blockSize
        then decrypted
        else
          let numInBlock = blockSize - (BS.length decrypted)
              curPaddingNum = BS.length decrypted + 1
              numOfByteAffected = BS.length encrCropped - (blockSize * 2) + numInBlock
              changedPreviousDecrypted = ""--map (xorBS x $ xorBS )
              allVariants = [BS.concat [(BS.take (numOfByteAffected - 1) encrCropped), BS.pack [x], changedPreviousDecrypted, BS.take blockSize (BS.drop (BS.length encrCropped - blockSize) encrCropped)] | x <- [0..255]]
              cForTrue [] = error "can't find"
              cForTrue allVar = if checkFun $ head allVariants then head allVariants else cForTrue $ tail allVar
              c' = cForTrue allVariants
              p = xorBS c' $ xorBS (BS.pack [1]) (BS.pack [(BS.unpack encrCropped) !! (numOfByteAffected - 1)])
          in c'
  in decryptLastBlock encrypted "" ""

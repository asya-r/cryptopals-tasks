{-# LANGUAGE OverloadedStrings #-}
module Ch9
  ( ch9
  , pkcs7
  ) where

import qualified Data.ByteString as BS
import Sound.OSC.Coding.Convert (int_to_word8)

ch9 :: BS.ByteString
ch9 = pkcs7 "YELLOW SUBMARINE" 20

pkcs7 :: BS.ByteString -> Int -> BS.ByteString
pkcs7 bs size =
  let paddingSize  = size - (BS.length bs)
      paddingBytes = BS.replicate paddingSize (int_to_word8 paddingSize)
  in BS.append bs paddingBytes

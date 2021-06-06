{-# LANGUAGE OverloadedStrings #-}
module PKCS7
  ( pkcs7
  , stripPadding
  ) where

import qualified Data.ByteString as BS
import Sound.OSC.Coding.Convert (int_to_word8, word8_to_int)

pkcs7 :: BS.ByteString -> Int -> BS.ByteString
pkcs7 bs size =
  let lastBlockSize = mod (BS.length bs) size
      paddingSize  = size - lastBlockSize
      paddingBytes = BS.replicate paddingSize (int_to_word8 paddingSize)
  in BS.append bs paddingBytes

stripPadding :: BS.ByteString -> Maybe BS.ByteString
stripPadding bs =
  let lastByte = word8_to_int $ BS.last bs
      mustBePadded = BS.take lastByte (BS.reverse bs)
      allEqual = BS.all (== BS.last bs) mustBePadded
      correctLen = (BS.length mustBePadded) == lastByte
      correctRange = lastByte /= 0
  in if allEqual && correctLen && correctRange
     then Just $ BS.reverse $ BS.drop lastByte (BS.reverse bs)
     else Nothing

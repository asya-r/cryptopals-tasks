{-# LANGUAGE OverloadedStrings #-}
module Ch2
  ( ch2
  , xorBS
  ) where

import Data.ByteString.Char8 (pack)
import Data.ByteString.Base16 (encode)
import qualified Data.ByteString as BS
import Data.Bits (xor)

import Utils (from16)

ch2 :: BS.ByteString
ch2 =  encode $ xorBS (from16 "1c0111001f010100061a024b53535009181c") (from16 "686974207468652062756c6c277320657965")

xorBS :: BS.ByteString -> BS.ByteString -> BS.ByteString
xorBS str1 str2 = BS.pack $ BS.zipWith xor str1 str2

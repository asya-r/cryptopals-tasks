{-# LANGUAGE OverloadedStrings #-}
module Ch1
    ( ch1
    , hexToBase64
    ) where

import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Base64 as Base64

import Utils (from16)

ch1 :: BS8.ByteString
ch1 = hexToBase64 "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"

hexToBase64 :: BS8.ByteString -> BS8.ByteString
hexToBase64 = Base64.encode . from16

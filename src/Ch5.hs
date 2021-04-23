{-# LANGUAGE OverloadedStrings #-}
module Ch5
  ( ch5
  , repeatingKeyXOR
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Bits (xor)
import Data.ByteString.Base16 (encode)

import Ch2 (xorBS)

ch5 :: BS.ByteString
ch5 = encode $ (repeatingKeyXOR . BS8.pack) "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal" "ICE"

repeatingKeyXOR :: BS.ByteString -> BS.ByteString -> BS.ByteString
repeatingKeyXOR str key =
  let str'    = BS.unpack str
      key'    = take (length str') (cycle $ BS.unpack key)
  in xorBS str (BS.pack key')

module Ch10
  (ch10
  ) where

import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Sound.OSC.Coding.Convert (int_to_word8)

import Ch7 (encryptAES_CBC, decryptAES_CBC)
import Utils (decodeB64)

ch10 :: String
ch10 =
  let key = BS8.pack "YELLOW SUBMARINE"
      iv = BS.replicate 16 0
      enc = decodeB64 $ unsafePerformIO $ readFile "files/10.txt"
      dec = decryptAES_CBC iv key enc
  in BS8.unpack dec

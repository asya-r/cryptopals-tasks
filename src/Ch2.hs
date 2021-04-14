module Ch2 (
  xorBS
  ) where

import Data.ByteString.Char8 (pack)
import Data.ByteString.Base16 (encode, decode)
import qualified Data.ByteString as BS
import Data.Bits (xor)

xorBS :: BS.ByteString -> BS.ByteString -> BS.ByteString
xorBS str1 str2 = encode $ BS.pack $ BS.zipWith xor (from16 str1) (from16 str2) where
  from16 = fst . decode

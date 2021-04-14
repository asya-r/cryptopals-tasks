module Ch1
    ( hexToBase64
    ) where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Base16 as Base16

hexToBase64 :: ByteString -> ByteString
hexToBase64 = Base64.encode . from16 where
  from16 = fst . Base16.decode

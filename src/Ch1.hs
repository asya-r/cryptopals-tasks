module Ch1
    ( hexToBase64
    ) where

import Data.ByteString.Char8 (pack, ByteString)
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Base16 as Base16

hexToBase64 :: String -> ByteString
hexToBase64 hexstr = case Base16.decode $ pack hexstr of
  Left s -> Base64.encode $ pack s
  Right bs -> Base64.encode bs

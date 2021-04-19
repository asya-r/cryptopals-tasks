module Ch5
  ( repeatingKeyXOR
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString.Base16 (encode)
import Data.Bits (xor)

repeatingKeyXOR :: String -> String -> BS.ByteString
repeatingKeyXOR str key =
  let strToW8 = BS.unpack . BS8.pack
      str'    = strToW8 str
      key'    = take (length str') (cycle $ strToW8 key)
  in encode $ BS.pack $ zipWith xor str' key'

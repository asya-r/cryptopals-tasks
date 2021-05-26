{-# LANGUAGE OverloadedStrings #-}
module Ch15
    ( ch15
    , stripPadding
    ) where

import qualified Data.ByteString as BS
import Sound.OSC.Coding.Convert (word8_to_int)

ch15 :: IO ()
ch15 = print $ stripPadding $ BS.concat ["ICE ICE BABY", BS.pack [1, 2, 3, 4]]

stripPadding :: BS.ByteString -> Maybe BS.ByteString
stripPadding bs =
  let lastByte = word8_to_int $ BS.last bs
      mustBePadded = BS.take lastByte (BS.reverse bs)
      allEqual = BS.all (== BS.last bs) mustBePadded
      correctLen = (BS.length mustBePadded) == lastByte
  in if allEqual && correctLen
     then Just $ BS.reverse $ BS.drop lastByte (BS.reverse bs)
     else Nothing

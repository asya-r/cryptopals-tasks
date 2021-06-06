{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module BitflipppingAttack
    ( bitflippingAttack
    , bitflippingServer
    ) where

import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString as BS
import Network.HTTP.Types.URI (parseQuery)

import AES (encryptAES_CBC, decryptAES_CBC)

bitflippingAttack :: BS.ByteString -> Int -> [Int] -> (BS.ByteString -> Bool) -> BS.ByteString
bitflippingAttack bs blockToChange bytesToChange fun = hlp $ allVariants (BS.take 16 $ BS.drop (16 * blockToChange) bs)
  where
    allVariants block = [BS.concat [BS.take (16 * blockToChange) bs, BS.take 5 block, BS.pack [x], BS.take 5 $ BS.drop 6 block, BS.pack [y], BS.drop 12 block, BS.drop (16 * (blockToChange+1)) bs] | x <- [0..255], y <- [0..255]]
    hlp [] = "sorry"
    hlp allVar = if fun $ head allVar then head allVar else hlp $ tail allVar

bitflippingServer :: String -> BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString
bitflippingServer mode str iv key = case mode of
    "get_ciphertext" -> withUserInput (BS8.unpack str) key iv
    "check_ciphertext" -> checkCiphertext str key iv
    "debug" -> decryptAES_CBC iv key str
    otherwise -> "what?"


withUserInput :: String -> BS.ByteString -> BS.ByteString -> BS.ByteString
withUserInput str key iv =
  let clean s = BS8.pack $ filter (`notElem` [';', '=']) s
      inserted x = BS.concat ["comment1=cooking%20MCs;userdata=", x, ";comment2=%20like%20a%20pound%20of%20bacon"]
      encrypted = encryptAES_CBC iv key (inserted $ clean str)
  in encrypted

checkCiphertext :: BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString
checkCiphertext bs key iv =
  let decrypted x = decryptAES_CBC iv key x
      admin query = filter (\(k, v) -> k == "admin") query
  in case admin $ parseQuery $ decrypted bs of
    [("admin", Just "true")] -> "admin"
    otherwise -> "not admin"

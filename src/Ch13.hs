{-# LANGUAGE OverloadedStrings #-}
module Ch13
    ( ch13
    ) where

import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString as BS
import Network.HTTP.Types.URI (parseQuery)

import Ch7 (encryptAES_ECB, decryptAES_ECB)
import Ch11 (generateKey)
import Ch15 (stripPadding)
import Ch9 (pkcs7)

ch13 :: IO ()
ch13 = do
  let admin_suf = BS.concat ["aaaaaaaaaa", pkcs7 "admin" 16, "a"]
      admin_suf_encrypted = BS.take 16 $ BS.drop 16 (BS8.pack $ server "get_profile" admin_suf)
      email = "aaaaaa@aaaa.a"
      valid_prefix_encrypted = BS.take 32 (BS8.pack $ server "get_profile" email)
  print $ server "get_role" (BS.concat [valid_prefix_encrypted, admin_suf_encrypted])

server :: String -> BS.ByteString -> String
server mode input =
  let key = "\146|\EM\v\189\&40\145\166\201\a\189Z(\128\216"
  in case mode of
    "get_profile" ->
      let query = profileToQuery $ profileFromEmail $ BS8.unpack input
          encrypted = encryptAES_ECB key query
      in BS8.unpack encrypted
    "get_role" ->
      let decrypted = stripPadding $ decryptAES_ECB key input
      in case decrypted of
        Just bs -> case getRole bs of
          Just "user" -> "ok, you are a user"
          Just "admin" -> "wow, admin!"
          Nothing -> "ugh, you are a cheater"
          otherwise -> "this role doesn`t exist"
        Nothing -> error "invalid string"

data Person = Person
  { email :: BS.ByteString
  , uid :: Int
  , role :: BS.ByteString
  } deriving Show

profileFromEmail :: String -> Person
profileFromEmail email =
  let clean email = BS8.pack $ filter (`notElem` ['&', '=']) email
  in Person { email = (clean email)
            , uid = 10
            , role = "user" }

profileToQuery :: Person -> BS.ByteString
profileToQuery person =
  BS.concat [ "email=", email person
            , "&uid=", BS8.pack $ show $ uid person
            , "&role=", role person ]

getRole :: BS.ByteString -> Maybe BS.ByteString
getRole bs =
  let query = parseQuery bs
      role = filter (\(k, v) -> k == "role") query
  in case length role of
    1 -> snd $ head role
    otherwise -> Nothing

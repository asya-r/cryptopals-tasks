{-# LANGUAGE OverloadedStrings #-}
module Ch13
    (
    ) where

import qualified Data.ByteString.Char8 as BS8
import Data.Aeson (Value(..), object, (.=), withObject, decode)
import Network.HTTP.Types.URI (parseQuery, Query)
import qualified Data.Text as T

createJSON :: Query -> Value
createJSON query =
  let parse item = (T.pack $ BS8.unpack $ fst item) .= (BS8.unpack <$> snd item)
  in object [parse x | x <- query]

profileFromEmail :: String -> Value
profileFromEmail email =
  let clean email = filter (`notElem` ['&', '=']) email
  in object [ "email" .= (clean email)
            , "uid" .= String "10"
            , "role" .= String "user" ]

-- createQuery :: Value -> Maybe Query
-- createQuery json = (decode json) :: Maybe [(BS8.ByteString, Maybe BS8.ByteString)]

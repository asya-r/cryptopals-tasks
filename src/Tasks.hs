{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Tasks
    ( tasks
    ) where

import qualified Data.ByteString as BS
import Data.ByteString.Base16 (encode)
import qualified Data.ByteString.Char8 as BS8
import System.Random (randomR, getStdRandom)
import Network.HTTP.Types.URI (parseQuery)

import Utils (hexToBase64, xorBS, from16, decodeB64)
import SingleByteXORAttack (findByteKey, findEncryptedString)
import RandomStuff (generateKey, generateKeyPrefix)
import AES (encryptAES_ECB, decryptAES_CBC, findAES_ECB, decryptAES_ECB)
import ByteAtATimeAttack (ecbDecrypt)
import PKCS7 (pkcs7, stripPadding)
import PaddingOracleAttack (serverEncrypt, serverCheckPadding, paddingOracleAttack)
import RandomStuff (generateIvKey, checkOracle)
import BitflipppingAttack (bitflippingAttack, bitflippingServer)
import CutAndPasteAttack (cutAndPasteAttack)
import RepeatingKeyXOR (repeatingKeyXOR, findKey)

tasks :: [IO ()]
tasks = [ ch1
        , ch2
        , ch3
        , ch4
        , ch5
        , ch6
        , ch7
        , ch8
        , ch9
        , ch10
        , ch11
        , ch12
        , ch13
        , ch14
        , ch15
        , ch16
        , ch17 ]

ch1 :: IO ()
ch1 = print $ hexToBase64 "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"

ch2 :: IO ()
ch2 =  print $ encode $ xorBS (from16 "1c0111001f010100061a024b53535009181c") (from16 "686974207468652062756c6c277320657965")

ch3 :: IO ()
ch3 = print (key, decrypted)
  where
    (key, decrypted) = findByteKey $ from16 "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"

ch4 :: IO ()
ch4 = do
  allStr <- lines <$> readFile "files/4.txt"
  print $ findEncryptedString $ encode <$> allStr
  where
    encode = from16 <$> BS8.pack

ch5 :: IO ()
ch5 = print $ encode $ (repeatingKeyXOR . BS8.pack) "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal" "ICE"

ch6 :: IO ()
ch6 = do
  file <- readFile "files/6.txt"
  print $ findKey $ BS8.pack file

ch7 :: IO ()
ch7 = do
  file <- readFile "files/7.txt"
  let key = "YELLOW SUBMARINE"
      encrypted = decodeB64 file
  print $ BS8.unpack $ decryptAES_ECB key encrypted

ch8 :: IO ()
ch8 = do
  allStr <- lines <$> readFile "files/8.txt"
  print $ findAES_ECB $ encode <$> allStr
  where
    encode = from16 . BS8.pack

ch9 :: IO ()
ch9 = print $ pkcs7 "YELLOW SUBMARINE" 20

ch10 :: IO ()
ch10 = do
  file <- readFile "files/10.txt"
  let key = BS8.pack "YELLOW SUBMARINE"
      iv = BS.replicate 16 0
      enc = decodeB64 file
      dec = decryptAES_CBC iv key enc
  print $ BS8.unpack dec

ch11 :: IO ()
ch11 = do
  (detected, actual) <- checkOracle
  print $ "Detected: " ++ detected ++ ", actual: " ++ actual

ch12 :: IO ()
ch12 = do
  encodedPlaintext <- readFile "files/12.txt"
  key <- generateKey
  let plaintext = decodeB64 encodedPlaintext
      oracleFunction (bs::BS.ByteString) = encryptAES_ECB key $ BS8.concat [bs, plaintext]
      decrypted = ecbDecrypt oracleFunction
  putStrLn decrypted

ch13 :: IO ()
ch13 = do
  key <- generateKey
  print $ cutAndPasteAttack key

ch14 :: IO ()
ch14 = do
  encodedPlaintext <- readFile "files/12.txt"
  (key, prefix) <- generateKeyPrefix
  let plaintext = decodeB64 encodedPlaintext
      oracleFunction (bs::BS.ByteString) = encryptAES_ECB key $ BS8.concat [prefix, bs, plaintext]
      decrypted = ecbDecrypt oracleFunction
  putStrLn decrypted

ch15 :: IO ()
ch15 = print $ stripPadding $ BS.concat ["ICE ICE BABY", BS.pack [1, 2, 3, 4]]

ch16 :: IO ()
ch16 = do
  (iv, key) <- generateIvKey
  let validPayload = BS.concat [BS.replicate 16 0, "aaaaaXadminXtrue"]
      validCiphertext = bitflippingServer "get_ciphertext" validPayload iv key
      blockToChange = 2
      bytesToChange = [6, 12]
      fun (bs::BS.ByteString) =
        let answer = bitflippingServer "check_ciphertext" bs iv key
        in if (BS.length answer) == 5 then True else False
  print $ parseQuery $ bitflippingServer "debug" (bitflippingAttack validCiphertext blockToChange bytesToChange fun) iv key

ch17 :: IO ()
ch17 = do
  (iv, key) <- generateIvKey
  strings <- lines <$> readFile "files/17.txt"
  lineNum <- getStdRandom (randomR (0,(length strings) - 1))
  let encrypted = serverEncrypt iv key (decodeB64 $ strings !! lineNum)
  print $ paddingOracleAttack iv encrypted (serverCheckPadding iv key)

{-# LANGUAGE OverloadedStrings #-}
module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import System.IO.Unsafe (unsafePerformIO)

import Ch1 (hexToBase64)
import Ch2 (xorBS)
import Ch3 (findByteKey)
import Ch4 (findEncryptedString)
import Ch5 (repeatingKeyXOR)
import Ch6 (hammingDistance)

data Task = Task
  { set       :: Int
  , challenge :: Int }

task :: Parser Task
task = Task
      <$> option auto
          ( long "set"
         <> short 's'
         <> help "Set number"
         <> metavar "INT" )
      <*> option auto
          ( long "challenge"
         <> short 'c'
         <> help "Challenge number"
         <> metavar "INT" )

main :: IO ()
main = check =<< execParser opts
 where
   opts = info (task <**> helper)
     ( fullDesc
    <> progDesc "Cryptopals tasks solutions" )

check :: Task -> IO ()
check (Task 1 1) = print $ hexToBase64 "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
check (Task 1 2) = print $ xorBS "1c0111001f010100061a024b53535009181c" "686974207468652062756c6c277320657965"
check (Task 1 3) = print $ findByteKey "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
check (Task 1 4) = print $ unsafePerformIO $ findEncryptedString "4.txt"
check (Task 1 5) = print $ repeatingKeyXOR "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal" "ICE"
check (Task 1 6) = print $ hammingDistance "this is a test" "wokka wokka!!!"
check _ = print "Haven't done this task yet"

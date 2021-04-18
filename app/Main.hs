{-# LANGUAGE OverloadedStrings #-}
module Main where

import Options.Applicative
import Data.Semigroup ((<>))

import Ch1 (hexToBase64)
import Ch2 (xorBS)

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
check _ = print "Haven't done this task yet"

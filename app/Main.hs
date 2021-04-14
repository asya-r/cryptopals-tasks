{-# LANGUAGE OverloadedStrings #-}
module Main where

import Ch1 (hexToBase64)
import Ch2 (xorBS)

main :: IO ()
main = do
  print $ hexToBase64 "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
  print $ xorBS "1c0111001f010100061a024b53535009181c" "686974207468652062756c6c277320657965"
  return ()

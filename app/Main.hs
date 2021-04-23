module Main where

import Options.Applicative
import Data.Semigroup ((<>))

import Ch1 (ch1)
import Ch2 (ch2)
import Ch3 (ch3)
import Ch4 (ch4)
import Ch5 (ch5)
import Ch6 (ch6)
import Utils (from16)

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
check (Task 1 1) = print ch1
check (Task 1 2) = print ch2
check (Task 1 3) = print ch3
check (Task 1 4) = print ch4
check (Task 1 5) = print ch5
check (Task 1 6) = print ch6
check _ = print "Haven't done this task yet"

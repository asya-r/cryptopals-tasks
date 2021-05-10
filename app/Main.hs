module Main where

import Options.Applicative
import Data.Semigroup ((<>))

import Ch1 (ch1)
import Ch2 (ch2)
import Ch3 (ch3)
import Ch4 (ch4)
import Ch5 (ch5)
import Ch6 (ch6)
import Ch7 (ch7)
import Ch8 (ch8)
import Ch9 (ch9)
import Ch10 (ch10)
import Ch11 (ch11)
import Ch12 (ch12)

data Task = Task
  { challenge :: Int }

task :: Parser Task
task = Task
      <$> option auto
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
check (Task 1) = print ch1
check (Task 2) = print ch2
check (Task 3) = print ch3
check (Task 4) = print ch4
check (Task 5) = print ch5
check (Task 6) = putStrLn ch6
check (Task 7) = putStrLn ch7
check (Task 8) = print ch8
check (Task 9) = print ch9
check (Task 10) = putStrLn ch10
check (Task 11) = ch11
check (Task 12) = ch12
check _ = print "Haven't done this task yet"

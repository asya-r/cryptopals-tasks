module Main where

import Options.Applicative
import Data.Semigroup ((<>))

import Tasks (tasks)

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
check (Task num) =
  if num `elem` [1..length tasks]
  then tasks !! (num - 1)
  else print "Haven't done this task yet"

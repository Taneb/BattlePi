module Main where

import System.Random

import BattlePi.Placement
import BattlePi.Loop

main :: IO ()
main = do
  g <- newStdGen
  print g
  let (b, g') = place g
  putStrLn "Do you want to go first or second?"
  r <- getLine
  let p = case r of
        "first" -> True
        "second" -> False
  battle p b g'

  

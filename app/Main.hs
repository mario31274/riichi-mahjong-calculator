module Main where

import Calculator
import Data.List (delete, nub, sort)
import Data.Maybe
import Hand
import Meld
import Parser
import Rule
import Tile
import Wall

main :: IO ()
main = do
  let hand = parse "45678p123s88m3p#C4p"
  print $ "You entered:"
  print $ hand
  print $ "The hand can be split into the following melds:"
  print $ map sort $ validMatches $ matchIntoMelds hand
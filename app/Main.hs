module Main where

import Calculator
import Data.List
import Data.Maybe
import Hand
import Meld
import Parser
import Rule
import Tile
import Wall

main :: IO ()
main = do
  mainLoop newCalculator
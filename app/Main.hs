module Main where

import Melds
import Rules
import Tiles
import Wall

hand :: Hand
hand =
  [ Numeric 2 Man,
    Numeric 2 Man,
    Numeric 3 Man,
    Numeric 3 Man,
    Numeric 4 Man,
    Numeric 4 Man,
    Numeric 5 Man,
    Numeric 5 Man,
    Numeric 6 Man,
    Numeric 6 Man,
    Numeric 7 Man,
    Numeric 7 Man,
    Numeric 8 Man,
    Numeric 8 Man
  ]

main :: IO ()
main = do
  let possibleCombinations = matchIntoMelds hand
  mapM_ print possibleCombinations
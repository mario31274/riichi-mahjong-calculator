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

tileRD :: Tile
tileRD = Dragon Red Honor

tileWD :: Tile
tileWD = Dragon White Honor

main :: IO ()
main = do
  -- let possibleCombinations = matchIntoMelds hand
  -- mapM_ print possibleCombinations
  -- print (cycleNext tileRD)

  print fullWall
  s <- shuffledWall
  print s
  let hand14 = take 14 s
  print hand14
  print (sortHand hand14)
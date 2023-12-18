module Main where

import Data.Maybe
import Hand
import Meld
import Tile
import Wall

hand :: Hand
hand =
  [ Numeric 2 Man,
    Numeric 2 Pin,
    Numeric 3 Man,
    Numeric 3 Pin,
    Numeric 4 Man,
    Numeric 4 Sou,
    Numeric 5 Man,
    Numeric 5 Man,
    Numeric 6 Man,
    Numeric 6 Man,
    Numeric 7 Man,
    Numeric 7 Man,
    Numeric 8 Man,
    Numeric 8 Man
  ]

hand2 :: (Hand, [Meld])
hand2 =
  ( [ Numeric 2 Man,
      Numeric 3 Man,
      Numeric 4 Man,
      Numeric 5 Man,
      Numeric 5 Man,
      Numeric 6 Man,
      Numeric 6 Man,
      Numeric 7 Man,
      Numeric 7 Man,
      Numeric 8 Man,
      Numeric 8 Man
    ],
    [Run (Numeric 2 Man) (Numeric 3 Man) (Numeric 4 Man) True]
  )

hand3 :: Hand
hand3 =
  [ Numeric 2 Man,
    Numeric 2 Man,
    Numeric 2 Man,
    Numeric 3 Man,
    Numeric 3 Man,
    Numeric 3 Man,
    Numeric 4 Man,
    Numeric 4 Man,
    Numeric 4 Man,
    Numeric 5 Man,
    Numeric 5 Man,
    Numeric 6 Man,
    Numeric 7 Man,
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

  -- print fullWall
  -- s <- shuffledWall
  -- print s
  -- let hand14 = take 14 s
  -- print hand14
  -- print (sortHand hand14)

  print hand3

  let u = filterUniquePermutations (validMatches (matchIntoMelds hand3))
  print u

-- let m = match hand (Triplet (Numeric 2 Man) (Numeric 3 Man) (Numeric 4 Man) False)
-- if m != Nothing then do
-- print m

-- let m2 = fromJust (pluck (Numeric 3 Man) hand)
-- let m2 = removeItem (Numeric 3 Man) hand
-- print m2

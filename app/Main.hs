module Main where

import Data.List (sort)
import Data.Maybe
import Match
import Meld
import Rule
import Tile
import Wall

hand2 :: Hand
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

hand1 :: Hand
hand1 =
  ( [ Numeric 2 Man,
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
      Numeric 9 Man,
      Numeric 9 Man
    ],
    []
  )

main :: IO ()
main = do
  let testHand = hand2
  let matchedHand = uniq $ sort $ map sort $ validMatches $ matchIntoMelds testHand
  print matchedHand

-- print $ isAllSimple (head matchedHand) (Numeric 1 Sou)

-- print $ map (\m -> isTwoSideWait m (Numeric 3 Man)) (head matchedHand)
-- print $ isTwoSideWait (Run (Numeric 1 Man) (Numeric 2 Man) (Numeric 3 Man) False) (Numeric 3 Man)
-- print $ isTwoSideWait (Run (Numeric 1 Man) (Numeric 2 Man) (Numeric 3 Man) False) (Numeric 1 Man)

-- print $ isClosedHand $ fst hand2
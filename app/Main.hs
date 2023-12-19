module Main where

import Data.List (nub, sort)
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

hand7 :: Hand
hand7 =
  ( [ Numeric 2 Man,
      Numeric 3 Man,
      Numeric 4 Man,
      Numeric 4 Man,
      Numeric 5 Man,
      Numeric 6 Man,
      Numeric 6 Man,
      Numeric 7 Man,
      Numeric 8 Man,
      Numeric 2 Sou,
      Numeric 3 Sou,
      Numeric 4 Sou,
      Numeric 9 Sou,
      Numeric 9 Sou
    ],
    []
  )

main :: IO ()
main = do
  -- print $ fullWall
  let testHand = hand7
  let matchedHand = uniq $ sort $ map sort $ validMatches $ matchIntoMelds testHand
  print matchedHand

-- print $ isAllSimple (head matchedHand) (Numeric 1 Sou)

-- print $ map (\m -> isTwoSideWait m (Numeric 3 Man)) (head matchedHand)
-- print $ isTwoSideWait (Run (Numeric 1 Man) (Numeric 2 Man) (Numeric 3 Man) False) (Numeric 3 Man)
-- print $ isTwoSideWait (Run (Numeric 1 Man) (Numeric 2 Man) (Numeric 3 Man) False) (Numeric 1 Man)
-- let matchedHand1 = uniq $ sort $ map sort $ validMatches $ matchIntoMelds hand1
-- print matchedHand1
-- print $ map (\ms -> isTwinSequences ms (Numeric 9 Man)) matchedHand1
-- print $ map (\ms -> isDoubleTwinSequences ms (Numeric 9 Man)) matchedHand1
-- print $ map (\ms -> isTwinSequences ms (Numeric 9 Man)) matchedHand
-- print $ fromEnum East

-- print $ isClosedHand $ fst hand2
module Main where

import Data.List (nub, sort)
import Data.Maybe
import Match
import Meld
import Rule
import Tile
import Wall

main :: IO ()
main = do
  -- print $ fullWall
  let testHand = hand9'
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
  print $ map (\ms -> isFullStraight ms (Numeric 9 Man)) matchedHand

-- print $ isClosedHand $ fst hand2
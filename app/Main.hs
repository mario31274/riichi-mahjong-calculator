module Main where

import Data.List (nub, sort)
import Data.Maybe
import Match
import Meld
import Parser
import Parser (parseTiles)
import Rule
import Tile
import Wall

hand6 :: Hand
hand6 =
  ( [ Numeric 9 Man,
      Numeric 9 Man,
      Numeric 9 Man,
      Numeric 2 Sou,
      Numeric 3 Sou,
      Numeric 7 Sou,
      Numeric 8 Sou,
      Numeric 9 Sou,
      Numeric 1 Sou,
      Dragon White Honor,
      Dragon White Honor
    ],
    [Run (Numeric 4 Sou) (Numeric 5 Sou) (Numeric 6 Sou) True]
  )

main :: IO ()
main = do
  -- print $ fullWall
  let testHand = hand6
  let matchedHand = uniq $ sort $ map sort $ validMatches $ matchIntoMelds testHand
  print matchedHand
  -- print $ map (\ms -> isFullStraight ms (Numeric 9 Man)) matchedHand

  -- print $ (getSingleTile "1z", getSingleTile "2z", getSingleTile "3z", getSingleTile "4z")
  -- print $ getOpenedMeld "C0s"
  -- print $ groupByDigits "44556678p123s88m3p"
  -- print $ groupByDigits "45678p123s88m3p#C456p"
  -- print $ parse "45678p123s88m3p#C456p"
  let p = groupByDigits "45678ps88m3p#C456p"
  print p
  let cs = groupTileStrings [] p
  print cs
  print $ parseTiles (fst cs)

-- print $ isClosedHand $ fst hand2
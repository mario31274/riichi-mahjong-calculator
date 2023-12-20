module Main where

import Data.List (nub, sort)
import Data.Maybe
import Hand
import Meld
import Parser
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
      Dragon White,
      Dragon White
    ],
    [Run (Numeric 4 Sou) (Numeric 5 Sou) (Numeric 6 Sou) True]
  )

main :: IO ()
main = do
  -- print $ fullWall
  -- let testHand = hand6
  -- let matchedHand = uniq $ sort $ map sort $ validMatches $ matchIntoMelds testHand
  -- print matchedHand
  -- print $ map (\ms -> isFullStraight ms (Numeric 9 Man)) matchedHand

  -- print $ (getSingleTile "1z", getSingleTile "2z", getSingleTile "3z", getSingleTile "4z")
  -- print $ getOpenedMeld "C0s"
  -- print $ groupByDigits "44556678p123s88m3p"
  -- print $ groupByDigits "45678p123s88m3p#C456p"
  -- print $ groupByDigits "234p55m#C2mP2sK2z"
  print $ groupTileAndMeldStrings "234p55mC2mP2sK2z"

  -- let hand9 = parser "45678p123s88m3pC4#K456p"
  -- print $ matchIntoMelds hand9
  -- print $ uniq $ sort $ map sort $ validMatches $ matchIntoMelds hand9
  -- print $ isValidWinningHand hand9
  let handS1 = "22233344455566m"
  let handS2 = "12345667778886p"
  let handS3 = "24m556677p999s66z3m"
  let handS4 = "33m333p678p333s33z3m"
  let hand = parse handS4

  -- let mss = uniq $ sort $ map sort $ validMatches $ matchIntoMelds hand
  -- print mss
  -- let found = findWinningMelds' mss (Numeric 6 Pin)
  -- print found
  -- print $ validMatches $ matchIntoMelds hand

  print $ getWinHandsByWinTile hand

-- print $ isClosedHand $ fst hand2
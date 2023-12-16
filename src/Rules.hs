module Rules where

import Data.List
import Melds
import Tiles
import Wall

data WinningHand = Standard Pair [Meld] | SevenPairs [Pair] | ThriteenOrphans [Tile]
  deriving (Eq, Show)

standard :: Hand -> WinningHand
-- standard h = Standard pair m1++m2++m3++m4
--   where m1
standard h = undefined

-- Function to classify tiles into meld types
classifyMeld :: [Tile] -> String
classifyMeld tiles
  | length tiles == 3 && isAllSame tiles = "SameThree"
  | length tiles == 4 && isAllSame tiles = "SameFour"
  | length tiles == 3 && isSequence tiles = "SequentialThree"
  | length tiles == 2 && isAllSame tiles = "Pair"
  | otherwise = "Invalid"

-- Function to match tiles into melds
matchIntoMelds :: [Tile] -> [[Tile]]
matchIntoMelds tiles =
  let possibleCombinations = subsequences tiles
   in filter (\combination -> classifyMeld combination /= "Invalid") possibleCombinations

-- -- Standard winning hand
-- standardWinningHand :: Hand -> WinningHand
-- standardWinningHand hand = Standard

-- -- All simple
-- isAllSimple :: WinningHand -> Bool
-- isAllSimple win = case win of
--   Standard p ms -> all ms isSequentialMeld

-- Pinfu / No-points hand

-- Seven Pairs

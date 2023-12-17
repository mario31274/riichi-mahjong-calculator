module Rules where

import Control.Monad (msum)
import Data.List
import Data.Maybe (fromMaybe)
import Melds
import Tiles
import Wall

data WinningHand = Standard [Meld] | SevenPairs [Meld] | ThriteenOrphans [Tile]
  deriving (Eq, Show)

matchStandard :: Hand -> WinningHand
-- standard h = Standard pair m1++m2++m3++m4
--   where m1
matchStandard h = undefined

-- Function to classify tiles into meld types
classifyMeld :: [Tile] -> String
classifyMeld tiles
  | length tiles == 3 && isAllSame tiles = "SameThree"
  | length tiles == 4 && isAllSame tiles = "SameFour"
  | length tiles == 3 && isSequence tiles = "SequentialThree"
  | length tiles == 2 && isAllSame tiles = "Pair"
  | otherwise = "Invalid"

-- Function to match tiles into melds
matchIntoMelds' :: [Tile] -> [[Tile]]
matchIntoMelds' tiles =
  let possibleCombinations = subsequences tiles
   in filter (\combination -> classifyMeld combination /= "Invalid") possibleCombinations

match :: Hand -> Meld -> Maybe Hand
match [] _ = Nothing
match hand (Triplet t1 t2 t3 _) = do
  h1 <- pluck t1 hand
  h2 <- pluck t2 h1
  pluck t3 h2
match hand (Pair t1 t2) = do
  h1 <- pluck t1 hand
  pluck t2 h1
match hand _ = Nothing

-- matchOneMeld :: Hand -> Maybe (Meld Hand)
-- matchIntoMelds (t:ts) = case seqMeld t of
--   Nothing -> matchIntoMelds ts
--   Just meld -> case match (t:ts) meld of
--     Just hand ->

matchIntoMelds :: Hand -> [Meld]
matchIntoMelds hand = go hand []
  where
    go :: Hand -> [Meld] -> [Meld]
    go [] melds = melds
    go h melds = case matchOneMeld h of
      Just (meld, newHand) -> go newHand (melds ++ [meld])
      Nothing -> go (tail h) melds -- Couldn't match any meld, try removing the first tile

-- Helper function to try matching a single meld
matchOneMeld :: Hand -> Maybe (Meld, Hand)
matchOneMeld hand = msum [matchTriplet hand, matchSequentialMeld hand, matchPair hand]

-- Helper function to try matching a triplet
matchPair :: Hand -> Maybe (Meld, Hand)
matchPair (t : ts) = do
  m <- pMeld t
  case match (t : ts) m of
    Nothing -> Nothing
    Just hand' -> Just (m, hand')
matchPair _ = Nothing

-- Helper function to try matching a triplet
matchTriplet :: Hand -> Maybe (Meld, Hand)
matchTriplet (t : ts) = do
  m <- triMeld t
  case match (t : ts) m of
    Nothing -> Nothing
    Just hand' -> Just (m, hand')
matchTriplet _ = Nothing

-- Helper function to try matching a sequential meld
matchSequentialMeld :: Hand -> Maybe (Meld, Hand)
matchSequentialMeld (t : ts) = do
  m <- seqMeld t
  case match (t : ts) m of
    Nothing -> Nothing
    Just hand' -> Just (m, hand')
matchSequentialMeld _ = Nothing

findWinningTile :: WinningHand -> Tile -> Maybe Meld
findWinningTile (Standard ms) = findTileInMelds ms
findWinningTile (SevenPairs ps) = findTileInMelds ps

findTileInMelds :: [Meld] -> Tile -> Maybe Meld
findTileInMelds [] _ = Nothing
findTileInMelds (m : ms) t
  | t `isTileInMeld` m = Just m
  | otherwise = findTileInMelds ms t

-- -- Standard winning hand
-- standardWinningHand :: Hand -> WinningHand
-- standardWinningHand hand = Standard

-- -- All simple
-- isAllSimple :: WinningHand -> Bool
-- isAllSimple win = case win of
--   Standard p ms -> all ms isSequentialMeld

-- Pinfu / No-points hand

-- Seven Pairs

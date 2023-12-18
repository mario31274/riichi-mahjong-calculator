module Rules where

import Control.Applicative
import Control.Monad (msum)
import Data.List
import Data.Maybe (fromMaybe)
import Melds
import Tiles
import Wall

data WinningHand = Standard [Meld] | SevenPairs [Meld] | ThirteenOrphans [Tile]
  deriving (Eq, Show)

matchStandard :: Hand -> WinningHand
-- standard h = Standard pair m1++m2++m3++m4
--   where m1
matchStandard h = undefined

pluck :: (Eq a) => a -> [a] -> Maybe [a]
pluck x [] = Nothing
pluck x (y : ys)
  | x == y = Just ys
  | otherwise = fmap (y :) (pluck x ys)

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
match hand (Run t1 t2 t3 _) = do
  h1 <- pluck t1 hand
  h2 <- pluck t2 h1
  pluck t3 h2
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

matchIntoMelds :: Hand -> ([Meld], [Tile])
matchIntoMelds hand = go hand ([], [])
  where
    go :: Hand -> ([Meld], [Tile]) -> ([Meld], [Tile])
    go [] (melds, tiles) = (melds, tiles)
    go h (melds, tiles) = case matchOneMeld h of
      Just (meld, newHand) -> go newHand (melds ++ [meld], tiles)
      Nothing -> go (tail h) (melds, tiles ++ [head h])

-- Helper function to try matching a single meld
matchOneMeld :: Hand -> Maybe (Meld, Hand)
-- matchOneMeld hand = matchTriplet hand <|> matchRun hand <|> matchPair hand
matchOneMeld hand = matchTriplet hand <|> matchRun hand

-- Helper function to try matching a pair
matchPair :: Hand -> Maybe (Meld, Hand)
matchPair = matchWith pMeld

-- Helper function to try matching a run
matchRun :: Hand -> Maybe (Meld, Hand)
matchRun = matchWith seqMeld

-- Helper function to try matching a triplet
matchTriplet :: Hand -> Maybe (Meld, Hand)
matchTriplet = matchWith triMeld

matchWith :: (Tile -> Maybe Meld) -> Hand -> Maybe (Meld, Hand)
matchWith meld (t : ts) = do
  m <- meld t
  case match (t : ts) m of
    Nothing -> Nothing
    Just hand' -> Just (m, hand')

-- matchHandWithMeld :: (Hand, [Meld]) ->

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

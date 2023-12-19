module Rule where

import Data.List
import Data.Maybe
import Match
import Meld
import Tile
import Wall

-- The two metrics for calculating final score
type HanFu = (Int, Int)

-- copy-pasted from Data.List.Unique module by Volodymyr Yashchenko
uniq :: (Eq b) => [b] -> [b]
uniq = map head . group

isThirteenOrphans :: Hand -> Bool
isThirteenOrphans (tiles, _) = all isTerminalTile tiles && length (uniq $ sort tiles) == 13

isValidWinningHand :: Hand -> Bool
isValidWinningHand (ts, ms)
  | null ms = length ts == 14
  | length ms == 1 = length ts == 11
  | length ms == 2 = length ts == 8
  | length ms == 3 = length ts == 5
  | length ms == 4 = length ts == 2
  | otherwise = False

-- Yakus
isClosedHand :: [Meld] -> Tile -> Bool
isClosedHand ms _ = all isClosedMeld ms

-- All simple
isAllSimple :: [Meld] -> Tile -> Bool
isAllSimple ms _ = all (all isNonTerminalTile . meldToTiles) ms

-- Pinfu / No-points hand  (closed or opened)
isPinfu :: [Meld] -> Tile -> Bool
isPinfu ms wt = do
  let threes = tail $ sort ms
  let winningMeld = findTileInMelds threes wt
  case winningMeld of
    Just (Run t1 t2 t3 False) -> isAllRuns threes && isTwoSideWait (Run t1 t2 t3 False) wt
    _ -> False
  where
    isAllRuns :: [Meld] -> Bool
    isAllRuns [] = True
    isAllRuns (m : ms) = case m of
      Run _ _ _ _ -> isAllRuns ms
      _ -> False

-- Twin Sequences
isTwinSequences :: [Meld] -> Tile -> Bool
isTwinSequences ms wt =
  all isClosedRun threes
    && length ms - length (nub ms) == 1
  where
    threes = tail $ sort ms

-- Double Twin Sequences
isDoubleTwinSequences :: [Meld] -> Tile -> Bool
isDoubleTwinSequences ms _ =
  all isClosedRun threes
    && length ms - length (nub ms) == 2
  where
    threes = tail $ sort ms

-- Three Mixed Sequences
isThreeMixedSequences :: [Meld] -> Tile -> Bool
isThreeMixedSequences ms _ = do
  let threes = tail $ sort ms
  let souM = find (\y -> suitOfMeld y == Just Sou) threes
  let pinM = find (\y -> suitOfMeld y == Just Pin) threes
  let manM = find (\y -> suitOfMeld y == Just Man) threes
  case souM of
    Just (Run s1 _ _ _) -> case pinM of
      Just (Run p1 _ _ _) -> case manM of
        Just (Run m1 _ _ _) -> numOf s1 == numOf p1 && numOf p1 == numOf m1
      _ -> False
    _ -> False

-- Full Straight
isFullStraight :: [Meld] -> Tile -> Bool
isFullStraight ms _ = do
  let threes = tail $ sort ms
  case find (\m -> numOfMeld m == Just 1) threes of
    Just m ->
      case find (\m' -> numOfMeld m' == Just 4 && suitOfMeld m == suitOfMeld m') threes of
        Just m ->
          case find (\m' -> numOfMeld m' == Just 7 && suitOfMeld m == suitOfMeld m') threes of
            Just m -> True
            _ -> False
        _ -> False
    _ -> False

-- Seven Pairs
isSevenPairs :: [Meld] -> Tile -> Bool
isSevenPairs melds _ = length (uniq melds) == 7

-- All Triplets (not Four)

-- Half Flush

-- Full Flush

-- All Honors

-- All Terminals

-- All Green

-- Fu's
-- Two-side Wait, if meld is open then return False
-- (Winning tiles can't be inside an opened meld)
isTwoSideWait :: Meld -> Tile -> Bool
isTwoSideWait (Run t1 t2 t3 False) t =
  (t == t1 && not (isTerminalTile t3)) || (t == t3 && not (isTerminalTile t1))
isTwoSideWait (Triplet t1 t2 t3 False) t = True
isTwoSideWait (Pair _ _) _ = False
isTwoSideWait _ _ = False

isOneSideWait :: Meld -> Tile -> Bool
isOneSideWait (Run _ t2 _ False) t = t == t2
isOneSideWait (Pair _ _) _ = True
isOneSideWait _ _ = False

isDragonPair :: Meld -> Bool
isDragonPair (Pair t1 _) = case t1 of
  (Dragon _ _) -> True
  _ -> False
isDragonPair _ = False

isWindPair :: Meld -> Bool
isWindPair (Pair t1 _) = case t1 of
  (Wind _ _) -> True
  _ -> False
isWindPair _ = False

isSelfWindPair :: Meld -> Wind -> Bool
isSelfWindPair (Pair t1 _) wind = case t1 of
  (Wind w _) -> w == wind
  _ -> False
isSelfWindPair _ _ = False
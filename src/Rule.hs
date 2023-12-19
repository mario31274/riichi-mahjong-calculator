module Rule where

import Data.List
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
  let winningMeld = findTileInMelds ms wt
  case winningMeld of
    Just (Run t1 t2 t3 False) -> helper threes && isTwoSideWait (Run t1 t2 t3 False) wt
    _ -> False
  where
    helper :: [Meld] -> Bool
    helper [] = True
    helper (m : ms) = case m of
      Run _ _ _ _ -> helper ms
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
isThreeMixedSequences ms _ = undefined

-- Seven Pairs
isSevenPairs :: [Meld] -> Bool
isSevenPairs melds = length (uniq melds) == 7

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
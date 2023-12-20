module Rule where

import Data.List
import Data.Maybe
import Hand
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

-- Yakus (Scoring Rules)
isClosedHand :: WinningHand -> Bool
isClosedHand w = all isClosedMeld (hand w)

-- All simple
isAllSimple :: WinningHand -> Bool
isAllSimple w = all (all isNonTerminalTile . meldToTiles) (hand w)

-- Pinfu / No-points hand  (closed or opened)
isPinfu :: WinningHand -> Bool
isPinfu w =
  let threes = tail $ sort $ hand w
   in case winningMeld w of
        (Run t1 t2 t3 False) ->
          isAllRuns threes
            && isTwoSideWait (Run t1 t2 t3 False) (winningTile w)
        _ -> False
  where
    isAllRuns :: [Meld] -> Bool
    isAllRuns [] = True
    isAllRuns (m : ms) = case m of
      Run _ _ _ _ -> isAllRuns ms
      _ -> False

-- Twin Sequences
isTwinSequences :: WinningHand -> Bool
isTwinSequences w =
  let ms = hand w
      threes = tail $ sort $ hand w
   in all isClosedRun threes
        && length ms - length (nub ms) == 1

-- Double Twin Sequences
isDoubleTwinSequences :: WinningHand -> Bool
isDoubleTwinSequences w =
  let ms = hand w
      threes = tail $ sort $ hand w
   in all isClosedRun threes
        && length ms - length (nub ms) == 2

-- Three Mixed Sequences
isThreeMixedSequences :: WinningHand -> Bool
isThreeMixedSequences w = do
  let threes = filter3TileMelds $ hand w
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
isAllTriplets :: WinningHand -> Bool
isAllTriplets = undefined

-- Half Flush
isHalfFlush :: WinningHand -> Bool
isHalfFlush = undefined

-- Full Flush
isFullFlush :: WinningHand -> Bool
isFullFlush = undefined

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
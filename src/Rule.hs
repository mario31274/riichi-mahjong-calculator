module Rule where

import Data.List
import Data.Maybe
import Meld
import Tile
import Wall

data WinningHand = WinningHand
  { hand :: [Meld],
    winningTile :: Tile,
    winningMeld :: Meld,
    isTsumo :: Bool,
    dora :: Int
  }
  deriving (Show, Ord, Eq)

defaultWH :: WinningHand
defaultWH =
  WinningHand
    { hand = [],
      winningTile = Default,
      winningMeld = Triplet Default Default Default False,
      isTsumo = False,
      dora = 0
    }

-- The two metrics for calculating final score
type HanFu = (Int, Int)

-- copy-pasted from Data.List.Unique module by Volodymyr Yashchenko
uniq :: (Eq b) => [b] -> [b]
uniq = map head . group

-- Yakus (Scoring Rules)
isClosedHand :: WinningHand -> Bool
isClosedHand w = all isClosedMeld (hand w)

-- Seven Pairs
isSevenPairs :: WinningHand -> Bool
isSevenPairs w = length (uniq (hand w)) == 7

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
      threes = filter3TileMelds ms
   in isClosedHand w
        && length ms - length (nub ms) == 1

-- Double Twin Sequences
isDoubleTwinSequences :: WinningHand -> Bool
isDoubleTwinSequences w =
  let ms = hand w
      threes = filter3TileMelds ms
   in isClosedHand w
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
isFullStraight :: WinningHand -> Bool
isFullStraight w = do
  let threes = filter3TileMelds $ hand w
  case find (\m -> numOfMeld m == Just 1) threes of
    Just m ->
      case find (\m' -> numOfMeld m' == Just 4 && suitOfMeld m == suitOfMeld m') threes of
        Just m ->
          case find (\m' -> numOfMeld m' == Just 7 && suitOfMeld m == suitOfMeld m') threes of
            Just m -> True
            _ -> False
        _ -> False
    _ -> False

-- All Triplets (not Four)
isAllTripletsYaku :: WinningHand -> Bool
isAllTripletsYaku w =
  let threes = filter3TileMelds $ hand w
   in all isTripletOrQuad threes -- && not isFourClosedTriplets w

-- Three Concealed Triplets
isThreeClosedTriplets :: WinningHand -> Bool
isThreeClosedTriplets w =
  let triplets = filterTripletOrQuadMelds $ hand w
   in length triplets == 3 && all isClosedMeld triplets

-- Three Mixed Triplets
isThreeMixedTriplets :: WinningHand -> Bool
isThreeMixedTriplets w = undefined

-- Three Quads
isThreeQuads :: WinningHand -> Bool
isThreeQuads w = undefined

-- All simple
isAllSimple :: WinningHand -> Bool
isAllSimple w = all (all isNonTerminalTile . meldToTiles) (hand w)

-- Honor Tiles
isHonorTiles :: WinningHand -> Bool
isHonorTiles w = undefined

-- Common Ends
isCommonEnds :: WinningHand -> Bool
isCommonEnds w = undefined

-- Common Terminals
isCommonTerminals :: WinningHand -> Bool
isCommonTerminals w = undefined

-- All Terminals and Honors
isAllTerminalsAndHonors :: WinningHand -> Bool
isAllTerminalsAndHonors w = undefined

-- Little Three Dragons
isLittleThreeDragons :: WinningHand -> Bool
isLittleThreeDragons w = undefined

-- Half Flush
isHalfFlush :: WinningHand -> Bool
isHalfFlush w = undefined

-- Full Flush
isFullFlush :: WinningHand -> Bool
isFullFlush w = undefined

-- Thirteen Orphans
isThirteenOrphans :: [Tile] -> Bool
isThirteenOrphans tiles = all isTerminalTile tiles && length (uniq $ sort tiles) == 13

-- Four Concealed Triplets
isFourClosedTriplets :: WinningHand -> Bool
isFourClosedTriplets w = undefined

-- Four Concealed Triplets Single Wait
isFourClosedTripletsSingleWait :: WinningHand -> Bool
isFourClosedTripletsSingleWait w = undefined

-- Big Three Dragons
isBigThreeDragons :: WinningHand -> Bool
isBigThreeDragons w = undefined

-- Little Four Winds
isLittleFourWinds :: WinningHand -> Bool
isLittleFourWinds w = undefined

-- Big Four Winds
isBigFourWinds :: WinningHand -> Bool
isBigFourWinds w = undefined

-- All Honors
isAllHonors :: WinningHand -> Bool
isAllHonors w = undefined

-- All Terminals
isAllTerminals :: WinningHand -> Bool
isAllTerminals w = undefined

-- All Green
isAllGreen :: WinningHand -> Bool
isAllGreen w = undefined

-- Nine Gates
isNineGates :: WinningHand -> Bool
isNineGates w = undefined

-- Nine Gates 9-wait
isNineGates9Waits :: WinningHand -> Bool
isNineGates9Waits w = undefined

-----------
-- Fu Related
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
  (Dragon _) -> True
  _ -> False
isDragonPair _ = False

isWindPair :: Meld -> Bool
isWindPair (Pair t1 _) = case t1 of
  (Wind _) -> True
  _ -> False
isWindPair _ = False

isSelfWindPair :: Meld -> Wind -> Bool
isSelfWindPair (Pair t1 _) wind = case t1 of
  (Wind w) -> w == wind
  _ -> False
isSelfWindPair _ _ = False
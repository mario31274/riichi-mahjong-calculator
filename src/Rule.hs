module Rule where

import Data.List
import Data.Maybe
import Meld
import Tile
import Wall

data Riichi = NoRiichi | SRiichi | DbRiichi
  deriving (Show, Ord, Eq)

data BonusAgari = NoBonus | DeadWallDrawAgari | RobbingAQuadAgari | UnderTheSeaAgari | UnderTheRiverAgari
  deriving (Show, Ord, Eq)

data WinningHand = WinningHand
  { hand :: [Meld],
    winningTile :: Tile,
    winningMeld :: Meld,
    isTsumo :: Bool,
    riichiStatus :: Riichi,
    isIppatsu :: Bool,
    bonusAragi :: BonusAgari,
    roundWind :: Wind,
    selfWind :: Wind,
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
      riichiStatus = NoRiichi,
      isIppatsu = False,
      bonusAragi = NoBonus,
      roundWind = East,
      selfWind = East,
      dora = 0
    }

-- copy-pasted from Data.List.Unique module by Volodymyr Yashchenko
uniq :: (Eq b) => [b] -> [b]
uniq = map head . group

isClosedHand :: WinningHand -> Bool
isClosedHand w = all isClosedMeld (hand w)

-- Yakus (Scoring Rules)

-- Riichi
isRiichi :: WinningHand -> Bool
isRiichi w = all isClosedMeld (hand w) && riichiStatus w == SRiichi

isClosedTsumo :: WinningHand -> Bool
isClosedTsumo w = all isClosedMeld (hand w) && isTsumo w

-- Ippatsu / One-shot
isIppatsuYaku :: WinningHand -> Bool
isIppatsuYaku w = (riichiStatus w /= NoRiichi) && isIppatsu w

-- Under the Sea
isUnderTheSea :: WinningHand -> Bool
isUnderTheSea w = bonusAragi w == UnderTheSeaAgari

-- Under the River
isUndertheRiver :: WinningHand -> Bool
isUndertheRiver w = bonusAragi w == UnderTheRiverAgari

-- Dead Wall Draw
isDeadWallDraw :: WinningHand -> Bool
isDeadWallDraw w = bonusAragi w == DeadWallDrawAgari

-- Robbing a Quad
isRobbingAQuad :: WinningHand -> Bool
isRobbingAQuad w = bonusAragi w == RobbingAQuadAgari

-- Double Riichi
isDoubleRiichi :: WinningHand -> Bool
isDoubleRiichi w = all isClosedMeld (hand w) && riichiStatus w == DbRiichi

-- Seven Pairs
isSevenPairs :: WinningHand -> Bool
isSevenPairs w = length (uniq (hand w)) == 7

-- No-points hand  (closed or opened)
isNoPointsHand :: WinningHand -> Bool
isNoPointsHand w =
  let threes = filter3TileMelds $ hand w
   in all isRun threes
        && isTwoSideWait (winningMeld w) (winningTile w)

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
isThreeMixedSequences w =
  let runs = filterRunMelds $ hand w
      souM = find (\y -> suitOfMeld y == Sou) runs
      pinM = find (\y -> suitOfMeld y == Pin) runs
      manM = find (\y -> suitOfMeld y == Man) runs
   in case souM of
        Just (Run s1 _ _ _) -> case pinM of
          Just (Run p1 _ _ _) -> case manM of
            Just (Run m1 _ _ _) ->
              numOf s1 == numOf p1
                && numOf p1 == numOf m1
            _ -> False
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
   in (length triplets == 3 && all isClosedMeld triplets)
        || ( length triplets == 4
               && not (isPair (winningMeld w))
               && not (isTsumo w)
           )

-- Three Mixed Triplets
isThreeMixedTriplets :: WinningHand -> Bool
isThreeMixedTriplets w =
  let triplets = filterTripletMelds $ hand w
      souM = find (\y -> suitOfMeld y == Sou) triplets
      pinM = find (\y -> suitOfMeld y == Pin) triplets
      manM = find (\y -> suitOfMeld y == Man) triplets
   in case souM of
        Just (Triplet s1 _ _ _) -> case pinM of
          Just (Triplet p1 _ _ _) -> case manM of
            Just (Triplet m1 _ _ _) ->
              numOf s1 == numOf p1
                && numOf p1 == numOf m1
            _ -> False
          _ -> False
        _ -> False

-- Three Quads
isThreeQuads :: WinningHand -> Bool
isThreeQuads w =
  let quads = filterQuadMelds $ hand w
   in length quads == 3

-- All simple
isAllSimple :: WinningHand -> Bool
isAllSimple w = all (all isNonTerminalTile . meldToTiles) (hand w)

isSelfWindTiles :: WinningHand -> Bool
isSelfWindTiles w =
  let melds = filterTripletOrQuadMelds $ hand w
   in any (isSelfWindMeld (selfWind w)) melds

-- Honor Tiles
isHonorTiles :: WinningHand -> Bool
isHonorTiles w =
  let quads = filterTripletOrQuadMelds $ hand w
   in any isHonorMeld quads

-- returns the list of honor tiles that led to the Honor Tiles Yaku
getHonorTiles :: WinningHand -> [Tile]
getHonorTiles w =
  if isHonorTiles w
    then
      ( let quads = filterTripletOrQuadMelds $ hand w
         in concatMap
              ( \m -> ([head (meldToTiles m) | suitOfMeld m == Honor])
              )
              quads
      )
    else []

-- Common Ends
isCommonEnds :: WinningHand -> Bool
isCommonEnds w =
  all isTerminalOrHonorMeld (hand w)

-- Common Terminals
isCommonTerminals :: WinningHand -> Bool
isCommonTerminals w =
  all isTerminalMeld (hand w)
    && not (any isHonorMeld (hand w))

-- All Terminals and Honors
isAllTerminalsAndHonors :: WinningHand -> Bool
isAllTerminalsAndHonors w =
  (all isTripletOrQuad (filter3TileMelds (hand w)) || all isPair (hand w))
    && all isTerminalOrHonorMeld (hand w)

-- Little Three Dragons
isLittleThreeDragons :: WinningHand -> Bool
isLittleThreeDragons w =
  let honorMs = filter isDragonMeld (hand w)
   in length (filter isPair honorMs) == 1
        && length (filter isTripletOrQuad honorMs) == 2

-- Half Flush
isHalfFlush :: WinningHand -> Bool
isHalfFlush w =
  let melds = map suitOfMeld (hand w)
   in length (nub $ filter (`elem` [Sou, Pin, Man]) melds) == 1
        && elem Honor melds

-- Full Flush
isFullFlush :: WinningHand -> Bool
isFullFlush w =
  let melds = map suitOfMeld (hand w)
   in length (nub $ filter (`elem` [Sou, Pin, Man]) melds) == 1

-- Thirteen Orphans
isThirteenOrphans :: WinningHand -> Bool
isThirteenOrphans w =
  all isTerminalOrHonorMeld (hand w)
    && length (filterSingle (hand w)) == 12
    && not (isPair (winningMeld w))

-- Thirteen Orphans 13-wait
isThirteenOrphans13Waits :: WinningHand -> Bool
isThirteenOrphans13Waits w =
  all isTerminalOrHonorMeld (hand w)
    && length (filterSingle (hand w)) == 12
    && isPair (winningMeld w)

-- Four Concealed Triplets
isFourClosedTriplets :: WinningHand -> Bool
isFourClosedTriplets w =
  let triplets = filterTripletOrQuadMelds $ hand w
   in length triplets == 4
        && all isClosedMeld triplets
        && not (isPair (winningMeld w))
        && isTsumo w

-- Four Concealed Triplets Single Wait
isFourClosedTripletsSingleWait :: WinningHand -> Bool
isFourClosedTripletsSingleWait w =
  let triplets = filterTripletOrQuadMelds $ hand w
   in length triplets == 4
        && all isClosedMeld triplets
        && isPair (winningMeld w)

-- Big Three Dragons
isBigThreeDragons :: WinningHand -> Bool
isBigThreeDragons w =
  let honorMs = filter isDragonMeld (hand w)
   in length (filter isTripletOrQuad honorMs) == 3

-- Little Four Winds
isLittleFourWinds :: WinningHand -> Bool
isLittleFourWinds w =
  let honorMs = filter isWindMeld (hand w)
   in length (filter isPair honorMs) == 1
        && length (filter isTripletOrQuad honorMs) == 3

-- Big Four Winds
isBigFourWinds :: WinningHand -> Bool
isBigFourWinds w =
  let honorMs = filter isWindMeld (hand w)
   in length (filter isTripletOrQuad honorMs) == 4

-- All Honors
isAllHonors :: WinningHand -> Bool
isAllHonors w =
  let tiles = concatMap meldToTiles $ hand w
   in all isHonorTile tiles

-- All Terminals
isAllTerminals :: WinningHand -> Bool
isAllTerminals w =
  let tiles = concatMap meldToTiles $ hand w
   in all isTerminalTile tiles

-- All Green
isAllGreen :: WinningHand -> Bool
isAllGreen w =
  let tiles = concatMap meldToTiles $ hand w
   in all isGreenTile tiles

-- Nine Gates
isNineGates :: WinningHand -> Bool
isNineGates w =
  let tiles = concatMap meldToTiles $ hand w
      suit = suitOfMeld $ winningMeld w
      toRemove = getNineGateTiles suit
      remain = deleteList toRemove tiles
   in length remain == 1

-- Nine Gates 9-wait
isNineGates9Waits :: WinningHand -> Bool
isNineGates9Waits w =
  let tiles = concatMap meldToTiles $ hand w
      suit = suitOfMeld $ winningMeld w
      toRemove = getNineGateTiles suit
      remain = sort (delete (winningTile w) tiles)
   in remain == toRemove

-- Helper functions for Nine Gates
deleteList :: (Eq a) => [a] -> [a] -> [a]
deleteList [] ys = ys
deleteList _ [] = []
deleteList (x : xs) ys =
  let ys' = delete x ys
   in deleteList xs ys'

getNineGateTiles :: Suit -> [Tile]
getNineGateTiles Honor = []
getNineGateTiles suit =
  [ Numeric 1 suit,
    Numeric 1 suit,
    Numeric 1 suit,
    Numeric 2 suit,
    Numeric 3 suit,
    Numeric 4 suit,
    Numeric 5 suit,
    Numeric 6 suit,
    Numeric 7 suit,
    Numeric 8 suit,
    Numeric 9 suit,
    Numeric 9 suit,
    Numeric 9 suit
  ]

-- Four Quads
isFourQuads :: WinningHand -> Bool
isFourQuads w =
  let quads = filterQuadMelds $ hand w
   in length quads == 4

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
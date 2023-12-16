module Melds where

import GHC.Event.Windows (Manager)
import System.Console.GetOpt (ArgDescr (NoArg))
import Tiles
import Wall

type Opened = Bool

data Pair = Pair Tile Tile
  deriving (Show, Ord, Eq)

data Meld = Triplet Tile Tile Tile Opened | Quad Tile Tile Tile Tile Opened
  deriving (Show, Ord, Eq)

pairMeld :: Tile -> Tile -> Maybe Pair
pairMeld t1 t2
  | isAllSame [t1, t2] = Just (Pair t1 t2)
  | otherwise = Nothing

sequentialMeld :: Tile -> Tile -> Tile -> Opened -> Maybe Meld
sequentialMeld t1 t2 t3 opened
  | isSequence [t1, t2, t3] = Just (Triplet t1 t2 t3 opened)
  | otherwise = Nothing

tripletMeld :: Tile -> Tile -> Tile -> Opened -> Maybe Meld
tripletMeld t1 t2 t3 opened
  | isAllSame [t1, t2, t3] = Just (Triplet t1 t2 t3 opened)
  | otherwise = Nothing

quadMeld :: Tile -> Tile -> Tile -> Tile -> Opened -> Maybe Meld
quadMeld t1 t2 t3 t4 opened
  | isAllSame [t1, t2, t3, t4] = Just (Quad t1 t2 t3 t4 opened)
  | otherwise = Nothing

pluck :: (Eq a) => a -> [a] -> Maybe (a, [a])
pluck x [] = Nothing
pluck x (y : ys)
  | x == y = Just (y, ys)
  | otherwise = case pluck x ys of
      Nothing -> Nothing
      Just (x', ys') -> Just (x', y : ys')

getSeqMeld :: Hand -> Maybe Meld
getSeqMeld (t : ts) = undefined

-- Closed sequential meld
seqMeld :: Tile -> Tile -> Tile -> Maybe Meld
seqMeld t1 t2 t3 = sequentialMeld t1 t2 t3 False

-- Closed triplet meld
triMeld :: Tile -> Maybe Meld
triMeld t1 = tripletMeld t1 t1 t1 False

-- --Unmodifiable melds--
--
-- Open sequential meld
chiMeld :: Tile -> Tile -> Tile -> Maybe Meld
chiMeld t1 t2 t3 = sequentialMeld t1 t2 t3 True

-- Open triplet meld
ponMeld :: Tile -> Tile -> Tile -> Maybe Meld
ponMeld t1 t2 t3 = tripletMeld t1 t2 t3 True

-- Open quadruple meld
openKanMeld :: Tile -> Tile -> Tile -> Tile -> Maybe Meld
openKanMeld t1 t2 t3 t4 = quadMeld t1 t2 t3 t4 True

-- Closed quadruple meld
closedKanMeld :: Tile -> Tile -> Tile -> Tile -> Maybe Meld
closedKanMeld t1 t2 t3 t4 = quadMeld t1 t2 t3 t4 False

-- Function to check if a list of tiles is a sequence
isSequence :: [Tile] -> Bool
isSequence [Numeric n1 s1, Numeric n2 s2, Numeric n3 s3] =
  n2 == n1 + 1 && n3 == n2 + 1 && all (== s1) [s2, s3]
isSequence _ = False

-- Function to check if a list of tiles is a triple or quad
isAllSame :: (Eq a) => [a] -> Bool
isAllSame (x : xs) = all (== x) xs
isAllSame _ = False

-- Function to check if a hand has a pair
hasPair :: Hand -> Bool
hasPair tiles = any (uncurry (==)) $ pairs tiles
  where
    pairs :: [a] -> [(a, a)]
    pairs [] = []
    pairs [_] = []
    pairs (x : y : xs) = (x, y) : pairs xs

-- Function to categorize a hand into sequences, triples, and a pair
-- categorizeWinningHand :: Hand -> ([Maybe Meld], [Maybe Meld], [Maybe Meld], [Maybe Meld])
-- categorizeWinningHand hand =
--   ([pairMeld], [sequentialMeld], [tripletMeld], [quadMeld])
--   where
--     sequences = filter isSequence (combinations 3 hand)
--     triples = filter isAllSame (combinations 3 hand)
--     pairs = filter isAllSame (combinations 2 hand)

-- Function to generate combinations of a certain size
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations k (x : xs) = map (x :) (combinations (k - 1) xs) ++ combinations k xs

-- All simple
isTanyaoMeld :: Meld -> Bool
isTanyaoMeld (Triplet t1 t2 t3 _) = all isSimpleTile [t1, t2, t3] && isSequence [t1, t2, t3]
isTanyaoMeld _ = False
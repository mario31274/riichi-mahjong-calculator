{-# LANGUAGE InstanceSigs #-}

module Meld where

import Tile
import Wall

type Opened = Bool

-- data Pair = Pair Tile Tile
--   deriving (Show, Ord, Eq)

data Meld = Single Tile | Pair Tile Tile | Run Tile Tile Tile Opened | Triplet Tile Tile Tile Opened | Quad Tile Tile Tile Tile Opened
  deriving (Eq)

instance Show Meld where
  show :: Meld -> String
  show m = case m of
    Pair t1 t2 -> show (t1, t2)
    Run t1 t2 t3 True -> "Chi" ++ show (t1, t2, t3)
    Run t1 t2 t3 False -> show (t1, t2, t3)
    Triplet t1 t2 t3 True -> "Pon" ++ show (t1, t2, t3)
    Triplet t1 t2 t3 False -> show (t1, t2, t3)
    Quad t1 t2 t3 t4 True -> "Kan" ++ show (t1, t2, t3, t4)
    Quad t1 t2 t3 t4 False -> "ClosedKan" ++ show (t1, t2, t3, t4)

instance Ord Meld where
  compare :: Meld -> Meld -> Ordering
  compare (Quad t1 _ _ _ False) (Quad t2 _ _ _ True) = LT

pairMeld :: Tile -> Tile -> Maybe Meld
pairMeld t1 t2
  | isAllSame [t1, t2] = Just (Pair t1 t2)
  | otherwise = Nothing

sequentialMeld :: Tile -> Tile -> Tile -> Opened -> Maybe Meld
sequentialMeld t1 t2 t3 opened
  | isSequence [t1, t2, t3] = Just (Run t1 t2 t3 opened)
  | otherwise = Nothing

tripletMeld :: Tile -> Tile -> Tile -> Opened -> Maybe Meld
tripletMeld t1 t2 t3 opened
  | isAllSame [t1, t2, t3] = Just (Triplet t1 t2 t3 opened)
  | otherwise = Nothing

quadMeld :: Tile -> Tile -> Tile -> Tile -> Opened -> Maybe Meld
quadMeld t1 t2 t3 t4 opened
  | isAllSame [t1, t2, t3, t4] = Just (Quad t1 t2 t3 t4 opened)
  | otherwise = Nothing

meldToTiles :: Meld -> [Tile]
meldToTiles (Pair t1 t2) = [t1, t2]
meldToTiles (Run t1 t2 t3 _) = [t1, t2, t3]
meldToTiles (Triplet t1 t2 t3 _) = [t1, t2, t3]
meldToTiles (Quad t1 t2 t3 t4 _) = [t1, t2, t3, t4]

isTileInMeld :: Tile -> Meld -> Bool
isTileInMeld t m = t `elem` meldToTiles m

pMeld :: Tile -> Maybe Meld
pMeld t = pairMeld t t

-- Closed sequential meld
seqMeld :: Tile -> Maybe Meld
seqMeld t = case getSeq t of
  Nothing -> Nothing
  Just (t1, t2, t3) -> sequentialMeld t1 t2 t3 False

getSeq :: Tile -> Maybe (Tile, Tile, Tile)
getSeq t1 = case nextNumeric t1 of
  Nothing -> Nothing
  Just t2 -> case nextNumeric t2 of
    Nothing -> Nothing
    Just t3 -> Just (t1, t2, t3)

-- Closed triplet meld
triMeld :: Tile -> Maybe Meld
triMeld t = tripletMeld t t t False

-- --Unmodifiable melds--
--
-- Open sequential meld
chiMeld :: Tile -> Maybe Meld
chiMeld t = case getSeq t of
  Nothing -> Nothing
  Just (t1, t2, t3) -> sequentialMeld t1 t2 t3 True

-- Open triplet meld
ponMeld :: Tile -> Maybe Meld
ponMeld t = tripletMeld t t t True

-- Open quadruple meld
openKanMeld :: Tile -> Maybe Meld
openKanMeld t = quadMeld t t t t True

-- Closed quadruple meld
closedKanMeld :: Tile -> Maybe Meld
closedKanMeld t = quadMeld t t t t False

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

-- Meld without terminal tiles
isTanyaoMeld :: Meld -> Bool
isTanyaoMeld (Triplet t1 t2 t3 _) = all isSimpleTile [t1, t2, t3] && isSequence [t1, t2, t3]
isTanyaoMeld _ = False
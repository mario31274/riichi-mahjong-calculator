module Melds where

import Tiles
import Wall

type Opened = Bool

data Meld = Single Tile | Pair Tile Tile Opened | Triplet Tile Tile Tile Opened | Quad Tile Tile Tile Tile Opened
  deriving (Show, Ord, Eq)

triplet :: Tile -> Tile -> Tile -> Opened -> Meld
triplet = Triplet

data SequenceMeld = SequenceMeld Tile Tile Tile
  deriving (Show, Eq)

sequenceMeld :: Tile -> Tile -> Tile -> Opened -> Maybe Meld
sequenceMeld t1 t2 t3 opened
  | isSequence [t1, t2, t3] = Just (Triplet t1 t2 t3 opened)
  | otherwise = Nothing

data TripletMeld = TripletMeld Tile Tile Tile
  deriving (Show, Eq)

tripletMeld :: Tile -> Tile -> Tile -> Opened -> Maybe Meld
tripletMeld t1 t2 t3 opened
  | isTriple [t1, t2, t3] = Just (Triplet t1 t2 t3 opened)
  | otherwise = Nothing

-- pairMeld :: (Tile, Tile) -> Maybe Meld
-- pairMeld pair = case pair of
--   (t, t) -> Nothing

-- chiMeld :: (Tile, Tile, Tile) -> Opened -> Meld
-- chiMeld t1 t2 t3 = Triplet (t1, t2, t3)

ponMeld :: Tile -> Tile -> Tile -> Opened -> Meld
ponMeld = Triplet

kanMeld :: Tile -> Tile -> Tile -> Tile -> Opened -> Meld
kanMeld = Quad

-- Function to check if a list of tiles is a sequence
isSequence :: [Tile] -> Bool
isSequence [Numeric n1 s1, Numeric n2 s2, Numeric n3 s3] =
  n2 == n1 + 1 && n3 == n2 + 1 && all (== s1) [s2, s3]
isSequence _ = False

-- Function to check if a list of tiles is a triple
isTriple :: [Tile] -> Bool
isTriple [t1, t2, t3] = t1 == t2 && t2 == t3
isTriple _ = False

-- Function to check if a hand has a pair
hasPair :: Hand -> Bool
hasPair tiles = any (\(x, y) -> x == y) $ pairs tiles
  where
    pairs :: [a] -> [(a, a)]
    pairs [] = []
    pairs [_] = []
    pairs (x : y : xs) = (x, y) : pairs xs

-- Function to categorize a hand into sequences, triples, and a pair
-- categorizeHand :: Hand -> ([(Tile, Tile, Tile)], [(Tile, Tile, Tile)], [(Tile, Tile)])
-- categorizeHand hand =
--   (sequences, triples, pairs)
--   where
--     sequences = filter isSequence (combinations 3 hand)
--     triples = filter isTriple (combinations 3 hand)
--     pairs = filter (uncurry (==)) (combinations 2 hand)

-- Function to generate combinations of a certain size
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations k (x : xs) = map (x :) (combinations (k - 1) xs) ++ combinations k xs

tanyaoMeld :: SequenceMeld
tanyaoMeld = undefined
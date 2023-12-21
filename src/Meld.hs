{-# LANGUAGE InstanceSigs #-}

module Meld where

import Tile

type Opened = Bool

data Meld = Pair Tile Tile | Run Tile Tile Tile Opened | Triplet Tile Tile Tile Opened | Quad Tile Tile Tile Tile Opened | Single Tile
  deriving (Ord, Eq)

instance Show Meld where
  show :: Meld -> String
  show m = case m of
    Single t -> show (t)
    Pair t1 t2 -> show (t1, t2)
    Run t1 t2 t3 True -> "Chi" ++ show (t1, t2, t3)
    Run t1 t2 t3 False -> show (t1, t2, t3)
    Triplet t1 t2 t3 True -> "Pon" ++ show (t1, t2, t3)
    Triplet t1 t2 t3 False -> show (t1, t2, t3)
    Quad t1 t2 t3 t4 True -> "Kan" ++ show (t1, t2, t3, t4)
    Quad t1 t2 t3 t4 False -> "ClosedKan(\x1f02b," ++ show t2 ++ "," ++ show t3 ++ ",\x1f02b)"

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
meldToTiles (Single t) = [t]
meldToTiles (Pair t1 t2) = [t1, t2]
meldToTiles (Run t1 t2 t3 _) = [t1, t2, t3]
meldToTiles (Triplet t1 t2 t3 _) = [t1, t2, t3]
meldToTiles (Quad t1 t2 t3 t4 _) = [t1, t2, t3, t4]

-- Extracts the number of the 1st tile in the meld
numOfMeld :: Meld -> Maybe Int
numOfMeld (Pair (Numeric n _) _) = Just n
numOfMeld (Run (Numeric n _) _ _ _) = Just n
numOfMeld (Triplet (Numeric n _) _ _ _) = Just n
numOfMeld (Quad (Numeric n _) _ _ _ _) = Just n
numOfMeld _ = Nothing -- non numeric tiles

-- Extracts the suid of the 1st tile in the meld
suitOfMeld :: Meld -> Suit
suitOfMeld (Pair (Numeric _ s) _) = s
suitOfMeld (Run (Numeric _ s) _ _ _) = s
suitOfMeld (Triplet (Numeric _ s) _ _ _) = s
suitOfMeld (Quad (Numeric _ s) _ _ _ _) = s
suitOfMeld _ = Honor -- non numeric tiles

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

singleMeld :: Tile -> Maybe Meld
singleMeld t
  | isTerminalOrHonorTile t = Just (Single t)
  | otherwise = Nothing

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

isClosedMeld :: Meld -> Bool
isClosedMeld m = case m of
  Pair _ _ -> True
  Run _ _ _ o -> not o
  Triplet _ _ _ o -> not o
  Quad _ _ _ _ o -> not o

isClosedRun :: Meld -> Bool
isClosedRun m = case m of
  Run _ _ _ False -> True
  _ -> False

-- Meld without terminal tiles
isNonTerminalMeld :: Meld -> Bool
isNonTerminalMeld (Run t1 t2 t3 _) = all isNonTerminalTile [t1, t2, t3]
isNonTerminalMeld m = isXMeld isNonTerminalTile m

isTerminalMeld :: Meld -> Bool
isTerminalMeld (Run t1 _ t3 _) = isTerminalTile t1 || isTerminalTile t3
isTerminalMeld m = isXMeld isTerminalTile m

isTerminalOrHonorMeld :: Meld -> Bool
isTerminalOrHonorMeld (Run t1 _ t3 _) = isTerminalOrHonorTile t1 || isTerminalOrHonorTile t3
isTerminalOrHonorMeld m = isXMeld isTerminalOrHonorTile m

isHonorMeld :: Meld -> Bool
isHonorMeld = isXMeld isHonorTile

isClosedTriplet :: Meld -> Bool
isClosedTriplet m = case m of
  Triplet _ _ _ False -> True
  _ -> False

isPair :: Meld -> Bool
isPair m = case m of
  Pair {} -> True
  _ -> False

isRun :: Meld -> Bool
isRun m = case m of
  Run {} -> True
  _ -> False

isTriplet :: Meld -> Bool
isTriplet m = case m of
  Triplet {} -> True
  _ -> False

isQuad :: Meld -> Bool
isQuad m = case m of
  Quad {} -> True
  _ -> False

isSingle :: Meld -> Bool
isSingle m = case m of
  Single _ -> True
  _ -> False

is3TileMeld :: Meld -> Bool
is3TileMeld m = isRun m || isTriplet m

isTripletOrQuad :: Meld -> Bool
isTripletOrQuad m = isTriplet m || isQuad m

isWindMeld :: Meld -> Bool
isWindMeld = isXMeld isWindTile

isDragonMeld :: Meld -> Bool
isDragonMeld = isXMeld isDragonTile

isSelfWindMeld :: Wind -> Meld -> Bool
isSelfWindMeld w = isXMeld (isXWindTile w)

isXMeld :: (Tile -> Bool) -> Meld -> Bool
isXMeld x (Single t) = x t
isXMeld x (Pair t1 _) = x t1
isXMeld x (Triplet t1 _ _ _) = x t1
isXMeld x (Quad t1 _ _ _ _) = x t1
isXMeld _ _ = False

filter3TileMelds :: [Meld] -> [Meld]
filter3TileMelds = filter is3TileMeld

filterRunMelds :: [Meld] -> [Meld]
filterRunMelds = filter isRun

filterTripletMelds :: [Meld] -> [Meld]
filterTripletMelds = filter isTriplet

filterQuadMelds :: [Meld] -> [Meld]
filterQuadMelds = filter isQuad

filterTripletOrQuadMelds :: [Meld] -> [Meld]
filterTripletOrQuadMelds = filter isTripletOrQuad

filterSingle :: [Meld] -> [Meld]
filterSingle = filter isSingle

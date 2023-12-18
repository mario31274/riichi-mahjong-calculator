module Match where

import Control.Applicative
import Data.List
import Data.Maybe (fromMaybe)
import Meld
import Tile
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

match :: [Tile] -> Meld -> Maybe [Tile]
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

matchIntoMelds :: Hand -> [([Meld], [Tile])]
matchIntoMelds (tiles, melds) = matchPattern (melds, tiles) patterns

matchPattern :: ([Meld], [Tile]) -> [[[Tile] -> Maybe (Meld, [Tile])]] -> [([Meld], [Tile])]
matchPattern (melds, tiles) [] = []
matchPattern (melds, tiles) (pattern : patterns) =
  matchOnePattern (melds, tiles) pattern : matchPattern (melds, tiles) patterns

matchOnePattern :: ([Meld], [Tile]) -> [[Tile] -> Maybe (Meld, [Tile])] -> ([Meld], [Tile])
matchOnePattern (melds, tiles) [] = (melds, tiles)
matchOnePattern (melds, tiles) (matcher : matchers) =
  case matcher tiles of
    Just (meld, tiles') -> matchOnePattern (melds ++ [meld], tiles') matchers
    Nothing -> (melds, tiles)

patterns :: [[[Tile] -> Maybe (Meld, [Tile])]]
patterns =
  [ [matchPair, match3, match3, match3, match3],
    [match3, matchPair, match3, match3, match3],
    [match3, match3, matchPair, match3, match3],
    [match3, match3, match3, matchPair, match3],
    [match3, match3, match3, match3, matchPair],
    [matchPair, match3', match3', match3', match3'],
    [match3', matchPair, match3', match3', match3'],
    [match3', match3', matchPair, match3', match3'],
    [match3', match3', match3', matchPair, match3'],
    [match3', match3', match3', match3', matchPair],
    [matchPair, matchPair, matchPair, matchPair, matchPair, matchPair, matchPair]
  ]

match3 :: [Tile] -> Maybe (Meld, [Tile])
match3 ts = matchRun ts <|> matchTriplet ts

match3' :: [Tile] -> Maybe (Meld, [Tile])
match3' ts = matchTriplet ts <|> matchRun ts

-- Helper function to try matching a pair
matchPair :: [Tile] -> Maybe (Meld, [Tile])
matchPair = matchWith pMeld

-- Helper function to try matching a run
matchRun :: [Tile] -> Maybe (Meld, [Tile])
matchRun = matchWith seqMeld

-- Helper function to try matching a triplet
matchTriplet :: [Tile] -> Maybe (Meld, [Tile])
matchTriplet = matchWith triMeld

matchWith :: (Tile -> Maybe Meld) -> [Tile] -> Maybe (Meld, [Tile])
matchWith meld (t : ts) = do
  m <- meld t
  case match (t : ts) m of
    Nothing -> Nothing
    Just tiles' -> Just (m, tiles')
matchWith meld [] = Nothing

validMatches :: [([Meld], [Tile])] -> [[Meld]]
validMatches [] = []
validMatches (match : matches) = case match of
  (m, []) -> m : validMatches matches
  _ -> validMatches matches

findWinningTile :: WinningHand -> Tile -> Maybe Meld
findWinningTile (Standard ms) = findTileInMelds ms
findWinningTile (SevenPairs ps) = findTileInMelds ps

findTileInMelds :: [Meld] -> Tile -> Maybe Meld
findTileInMelds [] _ = Nothing
findTileInMelds (m : ms) t
  | t `isTileInMeld` m = Just m
  | otherwise = findTileInMelds ms t

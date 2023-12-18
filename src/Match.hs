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

match :: Hand -> Meld -> Maybe Hand
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
match hand (Single t) = do
  pluck t hand
match hand _ = Nothing

matchIntoMelds :: Hand -> [([Meld], [Tile])]
matchIntoMelds hand = matchPattern ([], hand) patterns

matchPattern :: ([Meld], Hand) -> [[Hand -> Maybe (Meld, Hand)]] -> [([Meld], [Tile])]
matchPattern (melds, hand) [] = []
matchPattern (melds, hand) (pattern : patterns) =
  matchOnePattern (melds, hand) pattern : matchPattern (melds, hand) patterns

matchOnePattern :: ([Meld], Hand) -> [Hand -> Maybe (Meld, Hand)] -> ([Meld], [Tile])
matchOnePattern (melds, hand) [] = (melds, hand)
matchOnePattern (melds, hand) (matcher : matchers) =
  case matcher hand of
    Just (meld, newHand) -> matchOnePattern (melds ++ [meld], newHand) matchers
    Nothing -> (melds, hand)

patterns :: [[Hand -> Maybe (Meld, Hand)]]
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

match3 :: Hand -> Maybe (Meld, Hand)
match3 hand = matchRun hand <|> matchTriplet hand

match3' :: Hand -> Maybe (Meld, Hand)
match3' hand = matchTriplet hand <|> matchRun hand

-- Helper function to try matching a pair
matchPair :: Hand -> Maybe (Meld, Hand)
matchPair = matchWith pMeld

-- Helper function to try matching a run
matchRun :: Hand -> Maybe (Meld, Hand)
matchRun = matchWith seqMeld

-- Helper function to try matching a triplet
matchTriplet :: Hand -> Maybe (Meld, Hand)
matchTriplet = matchWith triMeld

matchWith :: (Tile -> Maybe Meld) -> Hand -> Maybe (Meld, Hand)
matchWith meld (t : ts) = do
  m <- meld t
  case match (t : ts) m of
    Nothing -> Nothing
    Just hand' -> Just (m, hand')
matchWith meld [] = Nothing

validMatches :: [([Meld], [Tile])] -> [[Meld]]
validMatches [] = []
validMatches (match : matches) = case match of
  (m, []) -> m : validMatches matches
  _ -> validMatches matches

filterUniquePermutations :: (Eq a, Ord a) => [a] -> [[a]]
filterUniquePermutations xs = nub $ map sort $ permutations xs

findWinningTile :: WinningHand -> Tile -> Maybe Meld
findWinningTile (Standard ms) = findTileInMelds ms
findWinningTile (SevenPairs ps) = findTileInMelds ps

findTileInMelds :: [Meld] -> Tile -> Maybe Meld
findTileInMelds [] _ = Nothing
findTileInMelds (m : ms) t
  | t `isTileInMeld` m = Just m
  | otherwise = findTileInMelds ms t

-- -- Standard winning hand
-- standardWinningHand :: Hand -> WinningHand
-- standardWinningHand hand = Standard

-- -- All simple
-- isAllSimple :: WinningHand -> Bool
-- isAllSimple win = case win of
--   Standard p ms -> all ms isSequentialMeld

-- Pinfu / No-points hand

-- Seven Pairs

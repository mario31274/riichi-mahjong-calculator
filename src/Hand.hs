module Hand where

import Control.Applicative
import Data.List
import Meld
import Rule
import Tile

type Hand = ([Tile], [Meld])

pluck :: (Eq a) => a -> [a] -> Maybe [a]
pluck x [] = Nothing
pluck x (y : ys)
  | x == y = Just ys
  | otherwise = fmap (y :) (pluck x ys)

pluckBy :: (Eq a) => (a -> a -> Bool) -> a -> [a] -> Maybe [a]
pluckBy f x [] = Nothing
pluckBy f x (y : ys)
  | f x y = Just ys
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
match hand (Single t) = do
  pluck t hand
match hand _ = Nothing

matchIntoMelds :: Hand -> [([Meld], [Tile])]
matchIntoMelds (tiles, melds) = matchPattern (melds, sort tiles) patterns

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
    [matchPair, matchPair, matchPair, matchPair, matchPair, matchPair, matchPair],
    [match13, match13, match13, match13, match13, match13, match13, match13, match13, match13, match13, match13, match13]
  ]

match3 :: [Tile] -> Maybe (Meld, [Tile])
match3 ts = matchRun ts <|> matchTriplet ts

match3' :: [Tile] -> Maybe (Meld, [Tile])
match3' ts = matchTriplet ts <|> matchRun ts

match13 :: [Tile] -> Maybe (Meld, [Tile])
match13 ts = matchPair ts <|> matchSingle ts

-- Helper function to try matching a pair
matchPair :: [Tile] -> Maybe (Meld, [Tile])
matchPair = matchWith pMeld

-- Helper function to try matching a run
matchRun :: [Tile] -> Maybe (Meld, [Tile])
matchRun = matchWith seqMeld

-- Helper function to try matching a triplet
matchTriplet :: [Tile] -> Maybe (Meld, [Tile])
matchTriplet = matchWith triMeld

-- Helper function to try matching a single tiles for Thirteen Orphans
matchSingle :: [Tile] -> Maybe (Meld, [Tile])
matchSingle = matchWith singleMeld

matchWith :: (Tile -> Maybe Meld) -> [Tile] -> Maybe (Meld, [Tile])
matchWith matcher (t : ts) = do
  m <- matcher t
  case match (t : ts) m of
    Nothing -> Nothing
    Just tiles' -> Just (m, tiles')
matchWith meld [] = Nothing

validMatches :: [([Meld], [Tile])] -> [[Meld]]
validMatches [] = []
validMatches (match : matches) = case match of
  (ms, []) -> case isValidWinningMelds ms of
    True -> ms : validMatches matches
    False -> validMatches matches
  _ -> validMatches matches

findTileInMelds :: [Meld] -> Tile -> Maybe Meld
findTileInMelds [] _ = Nothing
findTileInMelds (m : ms) t
  | t `isTileInMeld` m = Just m
  | otherwise = findTileInMelds ms t

isValidHand :: Hand -> Bool
isValidHand (ts, ms)
  | not $ noTilesMoreThanFour (ts, ms) = False
  | null ms = length ts == 14
  | length ms == 1 = length ts == 11
  | length ms == 2 = length ts == 8
  | length ms == 3 = length ts == 5
  | length ms == 4 = length ts == 2
  | otherwise = False

isValidWinningMelds :: [Meld] -> Bool
isValidWinningMelds ms = length ms == 5 || length ms == 7 || length ms == 13

noTilesMoreThanFour :: Hand -> Bool
noTilesMoreThanFour (ts, ms) = do
  let allTiles = ts ++ concatMap meldToTiles ms
   in all (\x -> length x <= 4) (group (sort allTiles))

getWinHandsByWinTile :: Hand -> [WinningHand]
getWinHandsByWinTile hand
  | not $ isValidHand hand = []
  | otherwise =
      let mss = map sort $ validMatches (matchIntoMelds hand)
          wt = last $ fst hand
          f = findWinningMelds mss wt
          sortedWH = sort $ map (\(ms, wm) -> initWinningHand ms wm wt) f
       in uniq sortedWH
  where
    initWinningHand :: [Meld] -> Meld -> Tile -> WinningHand
    initWinningHand ms wm t = defaultWH {hand = ms, winningMeld = wm, winningTile = t}

findWinningMelds :: [[Meld]] -> Tile -> [([Meld], Meld)]
findWinningMelds (ms : mss) t =
  let threesFirst = tail ms ++ [head ms]
   in findWinningMelds' (threesFirst : ms : mss) t
findWinningMelds [] _ = []

-- find the winning melds among winning hands with a win tile, ideally in closed melds
-- only return first occurrence
findWinningMelds' :: [[Meld]] -> Tile -> [([Meld], Meld)]
findWinningMelds' [] _ = []
findWinningMelds' (ms : mss) t = case findTileInMelds ms t of
  Just m ->
    (sort ms, m) : findWinningMelds' mss t
  Nothing -> findWinningMelds' mss t

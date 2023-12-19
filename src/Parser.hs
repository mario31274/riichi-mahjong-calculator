module Parser where

import Data.Char
import Meld
import Tile
import Wall

-- sanitize the input (Ns=Sou, Np=Pin, Nm=Man, Nz=Honor, CNx=Chi Meld, PNx=Pon Meld, KNx=Kan Meld, kNx=Closed Kan Meld)
sanitizeInputForTiles :: String -> String
sanitizeInputForTiles = filter (`elem` ['1' .. '9'] ++ ['s', 'p', 'm', 'z', 'C', 'P', 'K', 'k'])

parser :: String -> Hand
parser s =
  let (tss, mss) = groupTileAndMeldStrings s
   in (parseMaybe tss getSingleTile, parseMaybe mss getOpenedMeld)

parseMaybe :: [[Char]] -> ([Char] -> Maybe a) -> [a]
parseMaybe [] _ = []
parseMaybe (cs : css) p =
  case p cs of
    Just m -> m : parseMaybe css p
    Nothing -> parseMaybe css p

groupTileAndMeldStrings :: String -> ([String], [String])
groupTileAndMeldStrings s =
  let (tStrings, mStrings) = span isAlphaNum s
   in (groupTileStrings (groupByDigits tStrings), groupMeldStrings (groupByDigits mStrings))

groupTileStrings :: [String] -> [[Char]]
groupTileStrings [] = []
groupTileStrings (numbers : suit : ss)
  | not (all isDigit numbers) = groupTileStrings (suit : ss)
  | head suit `notElem` ['s', 'p', 'm', 'z'] = groupTileStrings (suit : ss)
  | otherwise = groupNumAndSuit numbers (head suit) ++ groupTileStrings ss
groupTileStrings (_ : _) = []

groupMeldStrings :: [String] -> [[Char]]
groupMeldStrings [] = []
groupMeldStrings (meldType : numbers : suit : ss)
  | head meldType `notElem` ['C', 'P', 'K', 'k']
      || not (isDigit $ head numbers)
      || head suit `notElem` ['s', 'p', 'm', 'z'] =
      groupMeldStrings (numbers : suit : ss)
  | otherwise = map head [meldType, numbers, suit] : groupMeldStrings ss
groupMeldStrings (_ : _ : _) = []
groupMeldStrings (_ : _) = []

groupNumAndSuit :: String -> Char -> [String]
groupNumAndSuit [] _ = []
groupNumAndSuit xs c = group xs
  where
    group :: String -> [String]
    group [] = []
    group (y : ys)
      | y `elem` ['1' .. '9'] = [y, c] : group ys
      | otherwise = [y] : group ys

groupByDigits :: String -> [String]
groupByDigits = group
  where
    group :: String -> [String]
    group [] = []
    group (c : cs)
      | isDigit c =
          let (digits, rest) = span isDigit cs
           in (c : digits) : group rest
      | isAlpha c =
          let (digits, rest) = span isAlpha cs
           in (c : digits) : group rest
      | otherwise = group cs

-- Takes only 2 first char and return a tile
-- Ns=Sou, Np=Pin, Nm=Man, Nz=Honor
getSingleTile :: [Char] -> Maybe Tile
getSingleTile (c1 : c2 : rest)
  | c2 == 's' && c1 `elem` ['1' .. '9'] = Just (Numeric (ord c1 - ord '0') Sou)
  | c2 == 'p' && c1 `elem` ['1' .. '9'] = Just (Numeric (ord c1 - ord '0') Pin)
  | c2 == 'm' && c1 `elem` ['1' .. '9'] = Just (Numeric (ord c1 - ord '0') Man)
  | c2 == 'z' && c1 `elem` ['1' .. '4'] = Just (Wind (toEnum ((ord c1 - ord '0') - 1)) Honor)
  | c2 == 'z' && c1 `elem` ['5' .. '7'] = Just (Dragon (toEnum ((ord c1 - ord '0' - 4) - 1)) Honor)
  | otherwise = Nothing
getSingleTile _ = Nothing

-- CNx=Chi Meld, PNx=Pon Meld, KNx=Kan Meld, kNx=Closed Kan Meld
getOpenedMeld :: [Char] -> Maybe Meld
getOpenedMeld (c1 : c2 : c3 : rest) = do
  case getSingleTile [c2, c3] of
    Just tile ->
      case c1 of
        'C' -> case tile of
          (Numeric 8 s) -> chiMeld (Numeric 7 s)
          (Numeric 9 s) -> chiMeld (Numeric 7 s)
          _ -> chiMeld tile
        'P' -> ponMeld tile
        'K' -> openKanMeld tile
        'k' -> closedKanMeld tile
        _ -> Nothing
    Nothing -> Nothing
getOpenedMeld _ = Nothing
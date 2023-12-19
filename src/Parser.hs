module Parser where

import Data.Char
import Meld
import Tile
import Wall

-- sanitize the input (Ns=Sou, Np=Pin, Nm=Man, Nz=Honor, CNx=Chi Meld, PNx=Pon Meld, KNx=Kan Meld, kNx=Closed Kan Meld)
sanitizeInputForTiles :: String -> String
sanitizeInputForTiles = filter (`elem` ['1' .. '9'] ++ ['s', 'p', 'm', 'z', 'C', 'P', 'K', 'k'])

-- parseHand :: String -> Hand
-- parseHand s = do
--   let grouped = groupByDigits s
--   let (tiles, rest) = parseTilesByGroup [] grouped
--   let melds = parseTilesByGroup rest
--   -- return (tiles, melds)
--   return ([],[])
--   -- where
parseTiles :: [[Char]] -> [Tile]
parseTiles [] = []
parseTiles (cs : css) =
  case getSingleTile cs of
    Just t -> t : parseTiles css
    Nothing -> parseTiles css

-- parseMelds :: [[Char]] -> [Meld]
-- parseMelds [] = []
-- parseMelds (cs : css) =
--   case getSingleTile cs of
--     Just t -> t : parseMelds css
--     Nothing -> parseMelds css

groupTileStrings :: [String] -> [String] -> ([[Char]], [String])
groupTileStrings s [] = (s, [])
groupTileStrings s (numbers : suit : ss)
  | not (all isDigit numbers) = (s, numbers : suit : ss)
  | head suit `notElem` ['s', 'p', 'm', 'z'] = (s, suit : ss)
  | otherwise = groupTileStrings (s ++ groupNumAndSuit numbers (head suit)) ss

-- groupMeldStrings :: [String] -> [[Char]]
-- groupMeldStrings [] = []
-- groupMeldStrings (meldType : numbers : suit : ss)
--   | head meldType `notElem` ['C', 'P', 'K', 'k'] = groupMeldStrings ss
--   | not (isDigit $ head numbers) = groupMeldStrings ss
--   | head suit `notElem` ['s', 'p', 'm', 'z'] = groupMeldStrings ss
--   | otherwise = groupTileStrings (s ++ groupMeldNumAndSuit numbers (head suit)) ss

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
      | isAlpha c = [c] : group cs
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
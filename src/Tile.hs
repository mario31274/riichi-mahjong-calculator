{-# LANGUAGE InstanceSigs #-}

module Tile where

import Data.Char

data Suit = Sou | Pin | Man | Honor
  deriving (Eq, Ord)

sou, pin, man, honor :: Suit
sou = Sou
pin = Pin
man = Man
honor = Honor

data Wind = East | South | West | North
  deriving (Eq, Enum, Bounded, Ord, Show)

data Dragon = White | Green | Red
  deriving (Eq, Enum, Bounded, Ord, Show)

data Tile = Numeric Int Suit | Wind Wind | Dragon Dragon | Default
  deriving (Eq)

instance Ord Tile where
  compare :: Tile -> Tile -> Ordering
  compare (Numeric n1 s1) (Numeric n2 s2)
    | s1 == s2 && n1 < n2 = LT
    | s1 == s2 && n1 == n2 = EQ
    | s1 == s2 && n1 > n2 = GT
    | s1 < s2 = LT
    | otherwise = GT
  compare (Wind w1) (Wind w2) = compare w1 w2
  compare (Dragon d1) (Dragon d2) = compare d1 d2
  compare (Numeric _ _) _ = LT
  compare (Wind _) (Numeric _ _) = GT
  compare (Wind _) (Dragon _) = LT
  compare (Dragon _) (Numeric _ _) = GT
  compare (Dragon _) (Wind _) = GT
  compare _ Default = LT
  compare Default _ = GT

numeric :: Int -> Suit -> Tile
numeric n suit
  | suit == Honor = error "no numeric honor tiles"
  | (n < 1) || (n > 9) = error "num out of bound"
  | otherwise = Numeric n suit

numericSuits :: [Suit]
numericSuits = [Sou, Pin, Man]

winds :: [Wind]
winds = [East ..]

dragons :: [Dragon]
dragons = [White ..]

wind :: Wind -> Tile
wind = Wind

dragon :: Dragon -> Tile
dragon = Dragon

nextNumeric :: Tile -> Maybe Tile
nextNumeric (Numeric n s)
  | n >= 1 || n <= 8 = Just (Numeric (n + 1) s)
  | otherwise = Nothing
nextNumeric _ = Nothing

cycleNext :: Tile -> Tile
cycleNext t = case t of
  Numeric n s
    | n >= 1 || n <= 8 -> Numeric (n + 1) s
    | otherwise -> Numeric 1 s
  Wind w -> Wind (succ' w)
  Dragon d -> Dragon (succ' d)

succ' :: (Bounded a, Eq a, Enum a) => a -> a
succ' n
  | n == maxBound = minBound
  | otherwise = succ n

isNonTerminalTile :: Tile -> Bool
isNonTerminalTile (Numeric n _)
  | (n < 2) || (n > 8) = False
  | otherwise = True
isNonTerminalTile _ = False

isTerminalTile :: Tile -> Bool
isTerminalTile (Numeric n _)
  | n == 1 || n == 9 = True
  | otherwise = False
isTerminalTile _ = False

isTerminalOrHonorTile :: Tile -> Bool
isTerminalOrHonorTile t = not $ isNonTerminalTile t

isHonorTile :: Tile -> Bool
isHonorTile t = suitOf t == Honor

isDragonTile :: Tile -> Bool
isDragonTile t = case t of
  Dragon _ -> True
  _ -> False

isWindTile :: Tile -> Bool
isWindTile t = case t of
  Wind _ -> True
  _ -> False

isXWindTile :: Wind -> Tile -> Bool
isXWindTile w t = case t of
  Wind w -> True
  _ -> False

isGreenTile :: Tile -> Bool
isGreenTile t = case t of
  Numeric 2 Sou -> True
  Numeric 3 Sou -> True
  Numeric 4 Sou -> True
  Numeric 6 Sou -> True
  Numeric 8 Sou -> True
  Dragon Green -> True
  _ -> False

instance Show Suit where
  show :: Suit -> String
  show suit = case suit of
    Sou -> "s"
    Pin -> "p"
    Man -> "m"
    Honor -> "z"

instance Show Tile where
  show :: Tile -> String
  show = unicodeShow

unicodeShow :: Tile -> String
unicodeShow tile = case tile of
  Numeric n suit -> case suit of
    Sou -> [chr (0x1f00f + n)]
    Pin -> [chr (0x1f018 + n)]
    Man -> [chr (0x1f006 + n)]
  Wind wind -> case wind of
    East -> "\x1f000"
    South -> "\x1f001"
    West -> "\x1f002"
    North -> "\x1f003"
  Dragon dragon -> case dragon of
    White -> "\x1f006"
    Green -> "\x1f005"
    Red -> "\x1f004"
  _ -> "\x1f02b"

asciiShow :: Tile -> String
asciiShow tile =
  case tile of
    Numeric n suit -> show n ++ show suit
    Wind w -> show (numOf tile) ++ show (suitOf $ Wind w)
    Dragon d -> show (numOf tile) ++ show (suitOf $ Dragon d)
    _ -> "??"

suitOf :: Tile -> Suit
suitOf tile = case tile of
  Numeric _ suit -> suit
  Wind _ -> Honor
  Dragon _ -> Honor

numOf :: Tile -> Int
numOf tile = case tile of
  Numeric n _ -> n
  Wind w -> fromEnum w + 1
  Dragon d -> fromEnum d + 5
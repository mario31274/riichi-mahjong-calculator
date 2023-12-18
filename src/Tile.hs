{-# LANGUAGE InstanceSigs #-}

module Tile where

import Data.Char
import Numeric (showHex)

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
  deriving (Eq, Enum, Bounded, Ord)

data Tile = Numeric Int Suit | Wind Wind Suit | Dragon Dragon Suit
  deriving (Eq)

instance Ord Tile where
  compare :: Tile -> Tile -> Ordering
  compare (Numeric n1 s1) (Numeric n2 s2)
    | s1 == s2 && n1 < n2 = LT
    | s1 == s2 && n1 == n2 = EQ
    | s1 == s2 && n1 > n2 = GT
    | s1 < s2 = LT
    | otherwise = GT
  compare (Wind w1 _) (Wind w2 _) = compare w1 w2
  compare (Dragon d1 _) (Dragon d2 _) = compare d1 d2
  compare (Numeric _ _) _ = LT
  compare (Wind _ _) (Dragon _ _) = LT
  compare (Wind _ _) (Numeric _ _) = GT
  compare (Dragon _ _) _ = GT

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
wind w = Wind w Honor

dragon :: Dragon -> Tile
dragon d = Dragon d Honor

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
  Wind w s -> Wind (succ' w) s
  Dragon d s -> Dragon (succ' d) s

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
isTerminalTile t = not $ isNonTerminalTile t

instance Show Suit where
  show suit = case suit of
    Sou -> "s"
    Pin -> "p"
    Man -> "m"
    Honor -> "z"

instance Show Tile where
  show = unicodeShow

unicodeShow :: Tile -> String
unicodeShow tile = case tile of
  Numeric n suit -> case suit of
    Sou -> [chr (0x1f00f + n)]
    Pin -> [chr (0x1f018 + n)]
    Man -> [chr (0x1f006 + n)]
  Wind wind _ -> case wind of
    East -> "\x1f000"
    South -> "\x1f001"
    West -> "\x1f002"
    North -> "\x1f003"
  Dragon dragon _ -> case dragon of
    White -> "\x1f006"
    Green -> "\x1f005"
    Red -> "\x1f004"

asciiShow :: Tile -> String
asciiShow tile =
  case tile of
    Numeric n suit -> show n ++ show suit
    Wind wind suit ->
      ( case wind of
          East -> "1"
          South -> "2"
          West -> "3"
          North -> "4"
      )
        ++ show suit
    Dragon dragon suit ->
      ( case dragon of
          White -> "5"
          Green -> "6"
          Red -> "7"
      )
        ++ show suit

suitOf :: Tile -> Suit
suitOf tile = case tile of
  Numeric _ suit -> suit
  Wind _ suit -> suit
  Dragon _ suit -> suit

module Player where

import Hand
import Tile

data Player = Player
  { score :: Int,
    hand :: Hand,
    discardedTiles :: [Tile],
    tenpaiState :: Tenpai
  }
  deriving (Show)

-- Final tile status
data Tenpai = NoTen | Tenpai | Riichi | Furiten
  deriving (Eq, Show)
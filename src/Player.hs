module Player where

import Tile
import Wall

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
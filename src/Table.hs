module Table where

import Player
import Tile
import Wall

data Board = Board
  { player1 :: Player,
    player2 :: Player,
    player3 :: Player,
    player4 :: Player,
    wall :: Wall,
    round :: Wind,
    player1Wind :: Wind,
    counterSticks :: Int
  }
  deriving (Show)
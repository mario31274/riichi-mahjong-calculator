module Table where

import Player
import Tile
import Wall

-- openPosition = 34*(3-(d+2)%4)+d*2
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
module Table where

import Data.Map

data Board = Board
  { players :: Player a,
    wall :: Wall a,
    round :: Round,
    roundHand :: RoundHand,
    roundBonus :: Integer
  }
  deriving (Show)
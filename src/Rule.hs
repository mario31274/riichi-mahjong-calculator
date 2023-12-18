module Rule where

import Data.List
import Meld
import Tile
import Wall

-- The two metrics for calculating final score
type HanFu = (Int, Int)

isSevenPairs :: [Meld] -> Bool
isSevenPairs melds = length (uniq melds) == 7

-- copy-pasted from Data.List.Unique module by Volodymyr Yashchenko
uniq :: (Eq b) => [b] -> [b]
uniq = map head . group

isThirteenOrphans :: Hand -> Bool
isThirteenOrphans (tiles, _) = all isTerminalTile tiles && length (uniq $ sort tiles) == 13
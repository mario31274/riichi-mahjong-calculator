module Rule where

import Data.List
import GHC.IO.Device (IODevice (isTerminal))
import Meld
import Tile
import Wall

isSevenPairs :: [Meld] -> Bool
isSevenPairs melds = length (uniq melds) == 7

-- copy-pasted from Data.List.Unique module by Volodymyr Yashchenko
uniq :: (Eq b) => [b] -> [b]
uniq = map head . group

isThirteenOrphans :: Hand -> Bool
isThirteenOrphans hand = all isTerminalTile hand && length (uniq $ sortHand hand) == 13
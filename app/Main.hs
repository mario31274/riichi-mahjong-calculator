module Main where

import Data.List (sort)
import Data.Maybe
import Match
import Meld
import Rule
import Tile
import Wall

hand1 :: Hand
hand1 =
  ( [ Numeric 2 Man,
      Numeric 2 Man,
      Numeric 3 Man,
      Numeric 3 Man,
      Numeric 4 Man,
      Numeric 4 Man,
      Numeric 5 Man,
      Numeric 5 Man,
      Numeric 6 Man,
      Numeric 6 Man,
      Numeric 7 Man,
      Numeric 7 Man,
      Numeric 8 Man,
      Numeric 8 Man
    ],
    []
  )

hand2 :: Hand
hand2 =
  ( [ Numeric 2 Man,
      Numeric 3 Man,
      Numeric 4 Man,
      Numeric 5 Man,
      Numeric 5 Man,
      Numeric 6 Man,
      Numeric 6 Man,
      Numeric 7 Man,
      Numeric 7 Man,
      Numeric 8 Man,
      Numeric 8 Man
    ],
    [Run (Numeric 2 Man) (Numeric 3 Man) (Numeric 4 Man) True]
  )

hand3 :: Hand
hand3 =
  ( [ Numeric 2 Man,
      Numeric 2 Man,
      Numeric 2 Man,
      Numeric 3 Man,
      Numeric 3 Man,
      Numeric 3 Man,
      Numeric 4 Man,
      Numeric 4 Man,
      Numeric 4 Man,
      Numeric 5 Man,
      Numeric 5 Man,
      Numeric 6 Man,
      Numeric 7 Man,
      Numeric 8 Man
    ],
    []
  )

hand4 :: Hand
hand4 =
  ( [ Numeric 2 Man,
      Numeric 2 Man,
      Numeric 2 Man,
      Numeric 2 Man,
      Numeric 3 Man,
      Numeric 3 Man,
      Numeric 3 Man,
      Numeric 3 Man,
      Numeric 4 Man,
      Numeric 4 Man,
      Numeric 4 Man,
      Numeric 4 Man,
      Numeric 5 Man,
      Numeric 5 Man
    ],
    []
  )

handThirteen :: Hand
handThirteen =
  ( [ Numeric 1 Pin,
      Numeric 9 Pin,
      Numeric 1 Sou,
      Numeric 9 Sou,
      Numeric 1 Man,
      Numeric 9 Man,
      Wind East Honor,
      Wind South Honor,
      Wind West Honor,
      Wind North Honor,
      Dragon White Honor,
      Dragon Green Honor,
      Dragon Green Honor,
      Dragon Red Honor
    ],
    []
  )

tileRD :: Tile
tileRD = Dragon Red Honor

tileWD :: Tile
tileWD = Dragon White Honor

main :: IO ()
main = do
  let testHand = hand1
  -- print (cycleNext tileRD)

  -- print fullWall
  -- s <- shuffledWall
  -- print s
  -- let hand14 = take 14 s
  -- print hand14
  -- print (sortHand hand14)

  print testHand
  print $ uniq $ sort $ map sort $ validMatches $ matchIntoMelds testHand

  print hand2
  print $ uniq $ sort $ map sort $ validMatches $ matchIntoMelds hand2

-- let m = fst $ matchOnePattern ([], testHand) [matchPair, matchPair, matchPair, matchPair, matchPair, matchPair, matchPair]
-- print $ uniq m
-- print $ isThirteenOrphans handThirteen

-- print $ uniq handThirteen

-- print $ isTerminalTile (Wind East Honor)

-- let m = match hand (Triplet (Numeric 2 Man) (Numeric 3 Man) (Numeric 4 Man) False)
-- if m != Nothing then do
-- print m

-- let m2 = fromJust (pluck (Numeric 3 Man) hand)
-- let m2 = removeItem (Numeric 3 Man) hand
-- print m2

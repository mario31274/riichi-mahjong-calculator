module Snippet where

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

hand5 :: Hand
hand5 =
  ( [ Numeric 1 Man,
      Numeric 1 Man,
      Numeric 1 Man,
      Numeric 1 Man,
      Numeric 2 Man,
      Numeric 2 Man,
      Numeric 2 Man,
      Numeric 2 Man,
      Numeric 3 Man,
      Numeric 3 Man,
      Numeric 3 Man,
      Numeric 3 Man,
      Numeric 4 Man,
      Numeric 4 Man
    ],
    []
  )

hand7 :: Hand
hand7 =
  ( [ Numeric 2 Man,
      Numeric 3 Man,
      Numeric 4 Man,
      Numeric 4 Man,
      Numeric 5 Man,
      Numeric 6 Man,
      Numeric 6 Man,
      Numeric 7 Man,
      Numeric 8 Man,
      Numeric 2 Sou,
      Numeric 3 Sou,
      Numeric 4 Sou,
      Numeric 9 Sou,
      Numeric 9 Sou
    ],
    []
  )

hand8 :: Hand
hand8 =
  ( [ Numeric 2 Man,
      Numeric 3 Man,
      Numeric 4 Man,
      Numeric 2 Pin,
      Numeric 3 Pin,
      Numeric 4 Pin,
      Numeric 2 Sou,
      Numeric 3 Sou,
      Numeric 4 Sou,
      Numeric 1 Pin,
      Numeric 1 Pin,
      Numeric 1 Pin,
      Wind South Honor,
      Wind South Honor
    ],
    []
  )

hand8' :: Hand
hand8' =
  ( [ Numeric 2 Pin,
      Numeric 3 Pin,
      Numeric 4 Pin,
      Numeric 2 Sou,
      Numeric 3 Sou,
      Numeric 4 Sou,
      Numeric 1 Pin,
      Numeric 1 Pin,
      Numeric 1 Pin,
      Wind South Honor,
      Wind South Honor
    ],
    [Run (Numeric 2 Man) (Numeric 3 Man) (Numeric 4 Man) True]
  )
hand9 :: Hand
hand9 =
  ( [ Numeric 9 Man,
      Numeric 9 Man,
      Numeric 9 Man,
      Numeric 2 Sou,
      Numeric 3 Sou,
      Numeric 4 Sou,
      Numeric 5 Sou,
      Numeric 6 Sou,
      Numeric 7 Sou,
      Numeric 8 Sou,
      Numeric 9 Sou,
      Numeric 1 Sou,
      Dragon White Honor,
      Dragon White Honor
    ],
    []
  )

hand9' :: Hand
hand9' =
  ( [ Numeric 9 Man,
      Numeric 9 Man,
      Numeric 9 Man,
      Numeric 2 Sou,
      Numeric 3 Sou,
      Numeric 7 Sou,
      Numeric 8 Sou,
      Numeric 9 Sou,
      Numeric 1 Sou,
      Dragon White Honor,
      Dragon White Honor
    ],
    [Run (Numeric 4 Sou) (Numeric 5 Sou) (Numeric 6 Sou) True]
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
  print (cycleNext tileRD)

  print fullWall
  s <- shuffledWall
  print s
  let hand14 = take 14 s
  print hand14
  print (sortHand hand14)

  print testHand
  print $ uniq $ sort $ map sort $ validMatches $ matchIntoMelds testHand

  print hand2
  print $ uniq $ sort $ map sort $ validMatches $ matchIntoMelds hand2

  let m = fst $ matchOnePattern ([], testHand) [matchPair, matchPair, matchPair, matchPair, matchPair, matchPair, matchPair]
  print $ uniq m
  print $ isThirteenOrphans handThirteen

  print $ uniq handThirteen

  print $ isTerminalTile (Wind East Honor)

  let m = match hand (Triplet (Numeric 2 Man) (Numeric 3 Man) (Numeric 4 Man) False)
  if m != Nothing then do
  print m

  let m2 = fromJust (pluck (Numeric 3 Man) hand)
  let m2 = removeItem (Numeric 3 Man) hand
  print m2


  print $ isAllSimple (head matchedHand) (Numeric 1 Sou)
  print $ map (\m -> isTwoSideWait m (Numeric 3 Man)) (head matchedHand)


  print $ map length matchedHand1
  print $ map (length . nub) matchedHand1
  print $ map (\ms -> length ms - 2 == length (nub ms) - 2) matchedHand1
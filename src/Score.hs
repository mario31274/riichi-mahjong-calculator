module Score where

import Data.List
import Data.Ord (comparing)
import Hand
import Meld
import Rule
import Text.ParserCombinators.ReadP (get)
import Tile

data Yaku = Normal Normal | Yakuman Yakuman
  deriving (Ord, Eq)

data Yakuhai = YakuhaiWind Wind | YakuhaiDragon Dragon
  deriving (Show, Ord, Eq)

data Normal
  = -- 1 Han yakus
    Riichi
  | Ippatsu
  | ClosedTsumo
  | AllSimple
  | HonorTiles Yakuhai
  | SelfWindTiles Wind
  | NoPointsHand
  | TwinSequences
  | DeadWallDraw
  | RobbingAQuad
  | UnderTheSea
  | UnderTheRiver
  | -- 2 Han yakus
    DoubleRiichi
  | SevenPairs
  | AllTriplets
  | ThreeClosedTriplets
  | ThreeQuads
  | ThreeMixedTriplets
  | AllTerminalsAndHonors
  | LittleThreeDragons
  | -- 2 Han yakus but 1 if opened
    ThreeMixedSequences Opened
  | FullStraight Opened
  | CommonEnds Opened
  | -- 3 Han yakus
    DoubleTwinSequences
  | -- 3 Han yakus but 2 if opened
    HalfFlush Opened
  | CommonTerminals Opened
  | -- 6 Han yakus
    FullFlush Opened
  | Dora Int
  deriving (Show, Ord, Eq)

data Yakuman
  = -- Forced Mankan
    NagashiMangan
  | -- Limit / Yakuman yakus
    ThirteenOrphans
  | ThirteenOrphans13Waits
  | FourClosedTriplets
  | FourClosedTripletsSingleWait
  | BigThreeDragons
  | LittleFourWinds
  | BigFourWinds
  | AllHonors
  | AllTerminals
  | AllGreen
  | NineGates
  | NineGates9Waits
  | FourQuads
  | BlessingOfHeaven
  | BlessingOfEarth
  | BlessingOfMan
  deriving (Show, Ord, Eq)

data Fu
  = Base -- 20 fu
  | ClosedRon -- 10 fu
  | SevenPairsBase -- 25 fu (includes Tsumo)
  | OpenNoPointsBase -- 30 fu
  | Tsumo -- 2 fu
  | SingleWait -- 2 fu
  | OpenSimpleTriplet Int -- 2 fu
  | ClosedSimpleTriplet Int -- 4 fu
  | OpenSimpleQuad Int -- 8 fu
  | ClosedSimpleQuad Int -- 16 fu
  | OpenTerminalTriplet Int -- 4 fu
  | ClosedTerminalTriplet Int -- 8 fu
  | OpenTerminalQuad Int -- 16 fu
  | ClosedTerminalQuad Int -- 32 fu
  | DragonPair -- 2 fu
  | WindPair -- 2 fu
  | SelfWindPair -- 2 fu
  deriving (Ord, Eq)

data Result = Result
  { yakus :: [Yaku],
    fus :: [Fu],
    han :: Int,
    fu :: Int,
    -- | The point that the loser needs to pay
    --   or the dealer needs to pay when tsumo'd
    total :: Int,
    -- | The point that the other two need to pay when tsumo'd
    tsumoTotal :: Int,
    -- | A copy of Hand for printing
    handCopy :: [Meld]
  }
  deriving (Eq)

instance Ord Result where
  compare r1 r2 = case compare (han r1) (han r2) of
    EQ -> compare (fu r1) (fu r2)
    other -> other

instance Show Result where
  show r
    | null (yakus r) =
        firstLines
          ++ "--------   This hand has no Yakus   --------\n"
          ++ "-=-=-=-=-=-=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-=-\n"
    | any isYakumanYaku (yakus r) =
        "-=-=-=-=-=-=-=-=-=-Result-=-=-=-=-=-=-=-=-=-\n"
          ++ concatMap (\m -> show m ++ " ") (handCopy r)
          ++ "\n "
          ++ yakumanLines
          ++ "\t\t\t"
          ++ scoreString
          ++ "------------        Yaku        ------------\n"
          ++ unlines (map (\y -> " " ++ show y ++ "  " ++ nTimesYakuman y) (filter isYakumanYaku (yakus r)))
          ++ "-=-=-=-=-=-=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-=-\n"
    | otherwise =
        firstLines
          ++ "------------        Yaku        ------------\n"
          ++ unlines (map (\y -> " " ++ show y ++ "    " ++ show (convertToHans y)) (yakus r))
          ++ "-=-=-=-=-=-=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-=-\n"
    where
      firstLines =
        "-=-=-=-=-=-=-=-=-=-Result-=-=-=-=-=-=-=-=-=-\n"
          ++ concatMap (\m -> show m ++ " ") (handCopy r)
          ++ "\n "
          ++ show (han r)
          ++ " Han / "
          ++ show (fu r)
          ++ " Fu\t\t\t"
          ++ scoreString
          ++ "------------         Fu         ------------\n"
          ++ unlines (map (\f -> " " ++ show f ++ "  " ++ show (convertToFus f)) (fus r))
      scoreString
        | tsumoTotal r == 0 = show (total r) ++ " pts\n"
        | total r == tsumoTotal r = show (total r) ++ " pts from all\n"
        | otherwise = show (total r) ++ " / " ++ show (tsumoTotal r) ++ " pts\n"
      yakumanLines
        | (han r) `div` 13 == 1 = "Yakuman"
        | otherwise = show ((han r) `div` 13) ++ "x Yakuman"
      nTimesYakuman y
        | (convertToHans y) `div` 13 == 1 = "Yakuman"
        | otherwise = show ((convertToHans y) `div` 13) ++ "x Yakuman"

instance Show Yaku where
  show y = case y of
    Normal Riichi -> "Riichi"
    Normal Ippatsu -> "Ippatsu"
    Normal ClosedTsumo -> "Closed Tsumo"
    Normal AllSimple -> "All Simple"
    Normal (HonorTiles yakuhai) -> "Honor Tiles " ++ show yakuhai
    Normal (SelfWindTiles wind) -> "Self Wind Tiles " ++ show wind
    Normal NoPointsHand -> "No Points Hand"
    Normal TwinSequences -> "Twin Sequences"
    Normal DeadWallDraw -> "Dead Wall Draw"
    Normal RobbingAQuad -> "Robbing A Quad"
    Normal UnderTheSea -> "Under The Sea"
    Normal UnderTheRiver -> "Under The River"
    Normal DoubleRiichi -> "Double Riichi"
    Normal SevenPairs -> "Seven Pairs"
    Normal AllTriplets -> "All Triplets"
    Normal ThreeClosedTriplets -> "Three Closed Triplets"
    Normal ThreeQuads -> "Three Quads"
    Normal ThreeMixedTriplets -> "Three Mixed Triplets"
    Normal AllTerminalsAndHonors -> "All Terminals And Honors"
    Normal LittleThreeDragons -> "Little Three Dragons"
    Normal (ThreeMixedSequences _) -> "Three Mixed Sequences"
    Normal (FullStraight _) -> "Full Straight"
    Normal (CommonEnds _) -> "Common Ends"
    Normal DoubleTwinSequences -> "Double Twin Sequences"
    Normal (HalfFlush _) -> "Half Flush"
    Normal (CommonTerminals _) -> "Common Terminals"
    Normal (FullFlush _) -> "Full Flush"
    Normal (Dora n) -> "Dora " ++ show n
    Yakuman NagashiMangan -> "Nagashi Mangan"
    Yakuman ThirteenOrphans -> "Thirteen Orphans"
    Yakuman ThirteenOrphans13Waits -> "13-Wait Thirteen Orphans"
    Yakuman FourClosedTriplets -> "Four Closed Triplets"
    Yakuman FourClosedTripletsSingleWait -> "Four Closed Triplets Single Wait"
    Yakuman BigThreeDragons -> "Big Three Dragons"
    Yakuman LittleFourWinds -> "Little Four Winds"
    Yakuman BigFourWinds -> "Big Four Winds"
    Yakuman AllHonors -> "All Honors"
    Yakuman AllTerminals -> "All Terminals"
    Yakuman AllGreen -> "All Green"
    Yakuman NineGates -> "Nine Gates"
    Yakuman NineGates9Waits -> "9-Wait Nine Gates"
    Yakuman FourQuads -> "Four Quads"
    Yakuman BlessingOfHeaven -> "Blessing Of Heaven"
    Yakuman BlessingOfEarth -> "Blessing Of Earth"
    Yakuman BlessingOfMan -> "Blessing Of Man"

instance Show Fu where
  show f = case f of
    Base -> "Base"
    ClosedRon -> "Closed Ron"
    SevenPairsBase -> "Seven Pairs Base"
    OpenNoPointsBase -> "No Points Hand Base"
    Tsumo -> "Tsumo"
    SingleWait -> "Single Tile Wait"
    OpenSimpleTriplet n -> "Open Simple Triplet x" ++ show n
    ClosedSimpleTriplet n -> "Closed Simple Triplet x" ++ show n
    OpenSimpleQuad n -> "Open Simple Quad x" ++ show n
    ClosedSimpleQuad n -> "Closed Simple Quad x" ++ show n
    OpenTerminalTriplet n -> "Open Terminal Triplet x" ++ show n
    ClosedTerminalTriplet n -> "Closed Terminal Triplet x" ++ show n
    OpenTerminalQuad n -> "Open Terminal Quad x" ++ show n
    ClosedTerminalQuad n -> "Closed Terminal Quad x" ++ show n
    DragonPair -> "Dragon Pair"
    WindPair -> "Wind Pair"
    SelfWindPair -> "Self Wind Pair"

calc :: [WinningHand] -> [Result]
calc [] = []
calc (w : whs) =
  let yakus = getYakus w
      fus = getFus
   in calcOneWinningHand w : calc whs

-- return a list of Yakus and Fu's of a winning hand
calcOneWinningHand :: WinningHand -> Result
calcOneWinningHand w =
  Result
    { yakus = yakus',
      fus = fus,
      han = han,
      fu = point,
      total = total,
      tsumoTotal = tsumoTotal,
      handCopy = hand w
    }
  where
    yakus = getYakus w
    yakus' = yakus ++ getDora w yakus
    fus = getFus w
    (han, point) = calcHansAndFus yakus' fus
    (total, tsumoTotal)
      | any isYakumanYaku yakus' = calcYakumanTotal (selfWind w == East) (isTsumo w) (han, point)
      | otherwise = calcNormalTotal (selfWind w == East) (isTsumo w) (han, point)

calcHansAndFus :: [Yaku] -> [Fu] -> (Int, Int)
calcHansAndFus yakus fus
  | any isYakumanYaku yakus =
      let yakus' = filter isYakumanYaku yakus
       in (calcYakus yakus', calcFus fus)
  | otherwise = (calcYakus yakus, calcFus fus)

getYakus :: WinningHand -> [Yaku]
getYakus w =
  appendYakus isThirteenOrphans [Yakuman ThirteenOrphans] w
    ++ appendYakus isThirteenOrphans13Waits [Yakuman ThirteenOrphans13Waits] w
    ++ appendYakus isFourClosedTriplets [Yakuman FourClosedTriplets] w
    ++ appendYakus isFourClosedTripletsSingleWait [Yakuman FourClosedTripletsSingleWait] w
    ++ appendYakus isBigThreeDragons [Yakuman BigThreeDragons] w
    ++ appendYakus isLittleFourWinds [Yakuman LittleFourWinds] w
    ++ appendYakus isBigFourWinds [Yakuman BigFourWinds] w
    ++ appendYakus isAllHonors [Yakuman AllHonors] w
    ++ appendYakus isAllTerminals [Yakuman AllTerminals] w
    ++ appendYakus isAllGreen [Yakuman AllGreen] w
    ++ appendYakus isNineGates [Yakuman NineGates] w
    ++ appendYakus isNineGates9Waits [Yakuman NineGates9Waits] w
    ++ appendYakus isFourQuads [Yakuman FourQuads] w
    ++ appendYakus isBlessingOfHeaven [Yakuman BlessingOfHeaven] w
    ++ appendYakus isBlessingOfEarth [Yakuman BlessingOfEarth] w
    ++ appendYakus isBlessingOfMan [Yakuman BlessingOfMan] w
    ++ appendYakus isNagashiMangan [Yakuman NagashiMangan] w
    ++ appendYakus isRiichi [Normal Riichi] w
    ++ appendYakus isIppatsu [Normal Ippatsu] w
    ++ appendYakus isClosedTsumo [Normal ClosedTsumo] w
    ++ appendYakus isAllSimple [Normal AllSimple] w
    ++ appendYakus isHonorTiles honorTilesYakus w
    ++ appendYakus isSelfWindTiles [selfWindYakus] w
    ++ appendYakus isNoPointsHand [Normal NoPointsHand] w
    ++ appendYakus isTwinSequences [Normal TwinSequences] w
    ++ appendYakus isDeadWallDraw [Normal DeadWallDraw] w
    ++ appendYakus isRobbingAQuad [Normal RobbingAQuad] w
    ++ appendYakus isUnderTheSea [Normal UnderTheSea] w
    ++ appendYakus isUnderTheRiver [Normal UnderTheRiver] w
    ++ appendYakus isDoubleRiichi [Normal DoubleRiichi] w
    ++ appendYakus isSevenPairs [Normal SevenPairs] w
    ++ appendYakus isAllTripletsYaku [Normal AllTriplets] w
    ++ appendYakus isThreeClosedTriplets [Normal ThreeClosedTriplets] w
    ++ appendYakus isThreeQuads [Normal ThreeQuads] w
    ++ appendYakus isThreeMixedTriplets [Normal ThreeMixedTriplets] w
    ++ appendYakus isAllTerminalsAndHonors [Normal AllTerminalsAndHonors] w
    ++ appendYakus isLittleThreeDragons [Normal LittleThreeDragons] w
    ++ appendYakus isThreeMixedSequencesClosed [Normal (ThreeMixedSequences False)] w
    ++ appendYakus isThreeMixedSequencesOpened [Normal (ThreeMixedSequences True)] w
    ++ appendYakus isFullStraightClosed [Normal (FullStraight False)] w
    ++ appendYakus isFullStraightOpened [Normal (FullStraight True)] w
    ++ appendYakus isCommonEndsClosed [Normal (CommonEnds False)] w
    ++ appendYakus isCommonEndsOpened [Normal (CommonEnds True)] w
    ++ appendYakus isDoubleTwinSequences [Normal DoubleTwinSequences] w
    ++ appendYakus isHalfFlushClosed [Normal (HalfFlush False)] w
    ++ appendYakus isHalfFlushOpened [Normal (HalfFlush True)] w
    ++ appendYakus isCommonTerminalsClosed [Normal (CommonTerminals False)] w
    ++ appendYakus isCommonTerminalsOpened [Normal (CommonTerminals True)] w
    ++ appendYakus isFullFlushClosed [Normal (FullFlush False)] w
    ++ appendYakus isFullFlushOpened [Normal (FullFlush True)] w
  where
    honorTilesYakus =
      zipWith
        (\_ t -> Normal (HonorTiles t))
        [1 ..]
        (getHonorTiles w)
    selfWindYakus = Normal $ SelfWindTiles $ selfWind w
    -- returns the list of honor tiles that led to the Honor Tiles Yaku
    getHonorTiles :: WinningHand -> [Yakuhai]
    getHonorTiles w =
      let tripOrQuads = filterTripletOrQuadMelds $ hand w
       in concatMap
            ( \m ->
                ( [ honorTileToYakuhai (head (meldToTiles m))
                    | suitOfMeld m == Honor
                        && windOfMeld m == Just (roundWind w)
                  ]
                )
            )
            tripOrQuads

    honorTileToYakuhai :: Tile -> Yakuhai
    honorTileToYakuhai t = case t of
      Wind w -> (YakuhaiWind w)
      Dragon w -> (YakuhaiDragon w)

getDora :: WinningHand -> [Yaku] -> [Yaku]
getDora w yakus
  | null yakus = []
  | dora w > 0 = [Normal $ Dora $ dora w]
  | otherwise = []

appendYakus :: (WinningHand -> Bool) -> [Yaku] -> WinningHand -> [Yaku]
appendYakus cond yaku w
  | cond w = yaku
  | otherwise = []

getFus :: WinningHand -> [Fu]
getFus w
  | isSevenPairs w = [SevenPairsBase]
  | isNoPointsHand w && isTsumo w = [Base]
  | isNoPointsHand w && not (isTsumo w) = [Base] ++ [ClosedRon]
  | otherwise =
      [Base]
        ++ appendFus isClosedRon ClosedRon w
        ++ appendFus isOneSideWait SingleWait w
        ++ appendFus isSevenPairs SevenPairsBase w
        ++ appendFus isOpenNoPoints OpenNoPointsBase w
        ++ appendFus isTsumo Tsumo w
        ++ appendMultiFus hasOpenSimpleTriplet (OpenSimpleTriplet 0) w
        ++ appendMultiFus hasClosedSimpleTriplet (ClosedSimpleTriplet 0) w
        ++ appendMultiFus hasOpenSimpleQuad (OpenSimpleQuad 0) w
        ++ appendMultiFus hasClosedSimpleQuad (ClosedSimpleQuad 0) w
        ++ appendMultiFus hasOpenTerminalTriplet (OpenTerminalTriplet 0) w
        ++ appendMultiFus hasClosedTerminalTriplet (ClosedTerminalTriplet 0) w
        ++ appendMultiFus hasOpenTerminalQuad (OpenTerminalQuad 0) w
        ++ appendMultiFus hasClosedTerminalQuad (ClosedTerminalQuad 0) w
        ++ appendFus isDragonPair DragonPair w
        ++ appendFus isWindPair WindPair w
        ++ appendFus isSelfWindPair SelfWindPair w

appendFus :: (WinningHand -> Bool) -> Fu -> WinningHand -> [Fu]
appendFus cond fu w
  | cond w = [fu]
  | otherwise = []

appendMultiFus :: (WinningHand -> Int) -> Fu -> WinningHand -> [Fu]
appendMultiFus cond fu w
  | num > 0 =
      case fu of
        OpenSimpleTriplet n -> [OpenSimpleTriplet num]
        ClosedSimpleTriplet n -> [ClosedSimpleTriplet num]
        OpenSimpleQuad n -> [OpenSimpleQuad num]
        ClosedSimpleQuad n -> [ClosedSimpleQuad num]
        OpenTerminalTriplet n -> [OpenTerminalTriplet num]
        ClosedTerminalTriplet n -> [ClosedTerminalTriplet num]
        OpenTerminalQuad n -> [OpenTerminalQuad num]
        ClosedTerminalQuad n -> [ClosedTerminalQuad num]
        _ -> []
  | otherwise = []
  where
    num = cond w

calcYakus :: [Yaku] -> Int
calcYakus yakus
  | any isYakumanYaku yakus =
      sum (map convertToHans (filter isYakumanYaku yakus))
  | otherwise = sum (map convertToHans yakus)

calcFus :: [Fu] -> Int
calcFus fus = case SevenPairsBase `elem` fus of
  True -> 25
  False ->
    roundUp $ sum (map convertToFus fus)
    where
      roundUp :: Int -> Int
      roundUp x = ceiling (fromIntegral x / 10) * 10

isYakumanYaku :: Yaku -> Bool
isYakumanYaku y = case y of
  Yakuman {} -> True
  _ -> False

isNormalYaku :: Yaku -> Bool
isNormalYaku y = case y of
  Normal {} -> True
  _ -> False

convertToHans :: Yaku -> Int
convertToHans y = case y of
  Normal yaku -> case yaku of
    Riichi -> 1
    Ippatsu -> 1
    ClosedTsumo -> 1
    AllSimple -> 1
    HonorTiles _ -> 1
    SelfWindTiles _ -> 1
    NoPointsHand -> 1
    TwinSequences -> 1
    DeadWallDraw -> 1
    RobbingAQuad -> 1
    UnderTheSea -> 1
    UnderTheRiver -> 1
    DoubleRiichi -> 2
    SevenPairs -> 2
    AllTriplets -> 2
    ThreeClosedTriplets -> 2
    ThreeQuads -> 2
    ThreeMixedTriplets -> 2
    AllTerminalsAndHonors -> 2
    LittleThreeDragons -> 2
    ThreeMixedSequences False -> 2
    ThreeMixedSequences True -> 1
    FullStraight False -> 2
    FullStraight True -> 1
    CommonEnds False -> 2
    CommonEnds True -> 1
    DoubleTwinSequences -> 3
    HalfFlush False -> 3
    HalfFlush True -> 2
    CommonTerminals False -> 3
    CommonTerminals True -> 2
    FullFlush False -> 6
    FullFlush True -> 5
    Dora n -> n
  Yakuman yaku -> case yaku of
    NagashiMangan -> 5
    ThirteenOrphans -> 13
    ThirteenOrphans13Waits -> 26
    FourClosedTriplets -> 13
    FourClosedTripletsSingleWait -> 26
    BigThreeDragons -> 13
    LittleFourWinds -> 13
    BigFourWinds -> 26
    AllHonors -> 13
    AllTerminals -> 13
    AllGreen -> 13
    NineGates -> 13
    NineGates9Waits -> 26
    FourQuads -> 13
    BlessingOfHeaven -> 13
    BlessingOfEarth -> 13
    BlessingOfMan -> 13

convertToFus :: Fu -> Int
convertToFus fu = case fu of
  Base -> 20
  ClosedRon -> 10
  SevenPairsBase -> 25
  OpenNoPointsBase -> 30
  Tsumo -> 2
  SingleWait -> 2
  OpenSimpleTriplet n -> 2 * n
  ClosedSimpleTriplet n -> 4 * n
  OpenSimpleQuad n -> 8 * n
  ClosedSimpleQuad n -> 16 * n
  OpenTerminalTriplet n -> 4 * n
  ClosedTerminalTriplet n -> 8 * n
  OpenTerminalQuad n -> 16 * n
  ClosedTerminalQuad n -> 32 * n
  DragonPair -> 2
  WindPair -> 2
  SelfWindPair -> 2

calcNormalTotal :: Bool -> Bool -> (Int, Int) -> (Int, Int)
calcNormalTotal isDealer isTsumo (han, fu)
  | isDealer && not isTsumo = case (han, fu) of
      (0, _) -> (0, 0)
      (1, 20) -> error "No (1, 20) possible when Ron"
      (1, 25) -> error "No (1, 25) possible when Ron"
      (1, 30) -> (1500, 0)
      (1, 40) -> (2000, 0)
      (1, 50) -> (2400, 0)
      (1, 60) -> (2900, 0)
      (1, 70) -> (3400, 0)
      (1, 80) -> (3900, 0)
      (1, 90) -> (4400, 0)
      (1, 100) -> (4800, 0)
      (1, 110) -> (5300, 0)
      (1, _) -> error "No (1, >110) possible when Ron"
      (2, 20) -> error "No (2, 20) possible when Ron"
      (2, 25) -> (2400, 0)
      (2, 30) -> (2900, 0)
      (2, 40) -> (3900, 0)
      (2, 50) -> (4800, 0)
      (2, 60) -> (5800, 0)
      (2, 70) -> (6800, 0)
      (2, 80) -> (7700, 0)
      (2, 90) -> (8700, 0)
      (2, 100) -> (9600, 0)
      (2, 110) -> (10600, 0)
      (2, _) -> (12000, 0)
      (3, 20) -> error "No (3, 20) possible when Ron"
      (3, 25) -> (4800, 0)
      (3, 30) -> (5800, 0)
      (3, 40) -> (7700, 0)
      (3, 50) -> (9600, 0)
      (3, 60) -> (11600, 0)
      (3, _) -> (12000, 0)
      (4, 20) -> error "No (4, 20) possible when Ron"
      (4, 25) -> (9600, 0)
      (4, 30) -> (11600, 0)
      (4, _) -> (12000, 0)
      (5, _) -> (12000, 0)
      (6, _) -> (18000, 0)
      (7, _) -> (18000, 0)
      (8, _) -> (24000, 0)
      (9, _) -> (24000, 0)
      (10, _) -> (24000, 0)
      (11, _) -> (36000, 0)
      (12, _) -> (36000, 0)
      _ -> (48000, 0)
  | isDealer && isTsumo = case (han, fu) of
      (0, _) -> (0, 0)
      (1, 30) -> (500, 500)
      (1, 40) -> (700, 700)
      (1, 50) -> (800, 800)
      (1, 60) -> (1000, 1000)
      (1, 70) -> (1200, 1200)
      (1, 80) -> (1300, 1300)
      (1, 90) -> (1500, 1500)
      (1, 100) -> (1600, 1600)
      (1, _) -> error "No (1, >100) possible when Tsumo"
      (2, 20) -> (700, 700)
      (2, 25) -> error "No (2, 25) possible when Tsumo"
      (2, 30) -> (1000, 1000)
      (2, 40) -> (1300, 1300)
      (2, 50) -> (1600, 1600)
      (2, 60) -> (2000, 2000)
      (2, 70) -> (2300, 2300)
      (2, 80) -> (2600, 2600)
      (2, 90) -> (2900, 2900)
      (2, 100) -> (3200, 3200)
      (2, 110) -> (3600, 3600)
      (2, _) -> (4000, 4000)
      (3, 20) -> (1300, 1300)
      (3, 25) -> (1600, 1600)
      (3, 30) -> (2000, 2000)
      (3, 40) -> (2600, 2600)
      (3, 50) -> (3200, 3200)
      (3, 60) -> (3900, 3900)
      (3, _) -> (4000, 4000)
      (4, 20) -> (2600, 2600)
      (4, 25) -> (3200, 3200)
      (4, 30) -> (3900, 3900)
      (4, _) -> (4000, 4000)
      (5, _) -> (4000, 4000)
      (6, _) -> (6000, 6000)
      (7, _) -> (6000, 6000)
      (8, _) -> (8000, 8000)
      (9, _) -> (8000, 8000)
      (10, _) -> (8000, 8000)
      (11, _) -> (12000, 12000)
      (12, _) -> (12000, 12000)
      _ -> (16000, 16000)
  | not isDealer && not isTsumo = case (han, fu) of
      (0, _) -> (0, 0)
      (1, 20) -> error "No (1, 20) possible when Ron"
      (1, 25) -> error "No (1, 25) possible when Ron"
      (1, 30) -> (1000, 0)
      (1, 40) -> (1300, 0)
      (1, 50) -> (1600, 0)
      (1, 60) -> (2000, 0)
      (1, 70) -> (2300, 0)
      (1, 80) -> (2600, 0)
      (1, 90) -> (2900, 0)
      (1, 100) -> (3200, 0)
      (1, 110) -> (3600, 0)
      (1, _) -> error "No (1, >110) possible when Ron"
      (2, 20) -> error "No (2, 20) possible when Ron"
      (2, 25) -> (1600, 0)
      (2, 30) -> (2000, 0)
      (2, 40) -> (2600, 0)
      (2, 50) -> (3200, 0)
      (2, 60) -> (3900, 0)
      (2, 70) -> (4500, 0)
      (2, 80) -> (5200, 0)
      (2, 90) -> (5800, 0)
      (2, 100) -> (6400, 0)
      (2, 110) -> (7100, 0)
      (2, _) -> (8000, 0)
      (3, 20) -> error "No (3, 20) possible when Ron"
      (3, 25) -> (3200, 0)
      (3, 30) -> (3900, 0)
      (3, 40) -> (5200, 0)
      (3, 50) -> (6400, 0)
      (3, 60) -> (7700, 0)
      (3, _) -> (8000, 0)
      (4, 20) -> error "No (4, 20) possible when Ron"
      (4, 25) -> (6400, 0)
      (4, 30) -> (7700, 0)
      (4, _) -> (8000, 0)
      (5, _) -> (8000, 0)
      (6, _) -> (12000, 0)
      (7, _) -> (12000, 0)
      (8, _) -> (16000, 0)
      (9, _) -> (16000, 0)
      (10, _) -> (16000, 0)
      (11, _) -> (24000, 0)
      (12, _) -> (24000, 0)
      _ -> (48000, 0)
  | otherwise = case (han, fu) of
      (0, _) -> (0, 0)
      (1, 30) -> (500, 300)
      (1, 40) -> (700, 400)
      (1, 50) -> (800, 400)
      (1, 60) -> (1000, 500)
      (1, 70) -> (1200, 600)
      (1, 80) -> (1300, 700)
      (1, 90) -> (1500, 800)
      (1, 100) -> (1600, 800)
      (1, _) -> error "No (1, >100) possible when Tsumo"
      (2, 20) -> (700, 400)
      (2, 25) -> error "No (2, 25) possible when Tsumo"
      (2, 30) -> (1000, 500)
      (2, 40) -> (1300, 700)
      (2, 50) -> (1600, 800)
      (2, 60) -> (2000, 1000)
      (2, 70) -> (2300, 1200)
      (2, 80) -> (2600, 1300)
      (2, 90) -> (2900, 1500)
      (2, 100) -> (3200, 1600)
      (2, 110) -> (3600, 1800)
      (2, _) -> (4000, 2000)
      (3, 20) -> (1300, 700)
      (3, 25) -> (1600, 800)
      (3, 30) -> (2000, 1000)
      (3, 40) -> (2600, 1300)
      (3, 50) -> (3200, 1600)
      (3, 60) -> (3900, 2000)
      (3, _) -> (4000, 2000)
      (4, 20) -> (2600, 1300)
      (4, 25) -> (3200, 1600)
      (4, 30) -> (3900, 2000)
      (4, _) -> (4000, 2000)
      (5, _) -> (4000, 2000)
      (6, _) -> (6000, 3000)
      (7, _) -> (6000, 3000)
      (8, _) -> (8000, 4000)
      (9, _) -> (8000, 4000)
      (10, _) -> (8000, 4000)
      (11, _) -> (12000, 6000)
      (12, _) -> (12000, 6000)
      _ -> (16000, 8000)

calcYakumanTotal :: Bool -> Bool -> (Int, Int) -> (Int, Int)
calcYakumanTotal isDealer isTsumo (han, fu)
  | isDealer && not isTsumo = case (han, fu) of
      (h, _) -> (h `div` 13 * 48000, 0)
  | isDealer && isTsumo = case (han, fu) of
      (h, _) -> (h `div` 13 * 16000, h `div` 13 * 16000)
  | not isDealer && not isTsumo = case (han, fu) of
      (h, _) -> (h `div` 13 * 32000, 0)
  | otherwise = case (han, fu) of
      (h, _) -> (h `div` 13 * 16000, h `div` 13 * 8000)

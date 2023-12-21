module Calculator where

import Hand
import Rule
import Tile

type Calculator a = String -> IO a

-- The two metrics for calculating final score
type HanFu = (Int, Int)

data Yaku = Normal Normal | Yakuman Yakuman

data Normal
  = -- 1 Han yakus
    Riichi
  | Ippatsu
  | ClosedTsumo
  | AllSimple
  | HonorTiles Tile
  | SelfWindTiles Tile
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
    ThreeMixedSequences
  | ThreeMixedSequencesOpened
  | FullStraight
  | FullStraightOpened
  | CommonEnds
  | CommonEndsOpened
  | -- 3 Han yakus
    DoubleTwinSequences
  | -- 3 Han yakus but 2 if opened
    HalfFlush
  | HalfFlushOpened
  | CommonTerminals
  | CommonTerminalsOpened
  | -- 6 Han yakus
    FullFlush
  | FullFlushOpened
  | Dora Int

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

data Fu
  = Base -- 20 fu
  | ClosedRon -- 10 fu
  | SevenPairsBase -- 25 fu (includes Tsumo)
  | OpenNoPointsBase -- 30 fu
  | Tsumo -- 2 fu
  | SingleWait -- 2 fu
  | OpenSimpleTriplet -- 2 fu
  | ClosedSimpleTriplet -- 4 fu
  | OpenSimpleQuad -- 8 fu
  | ClosedSimpleQuad -- 16 fu
  | OpenTerminalTriplet -- 4 fu
  | ClosedTerminalTriplet -- 8 fu
  | OpenTerminalQuad -- 16 fu
  | ClosedTerminalQuad -- 32 fu
  | DragonPair -- 2 fu
  | WindPair -- 2 fu
  | SelfWindPair -- 2 fu

calc :: [WinningHand] -> [([Yaku], [Fu], (Int, Int), Int)]
calc hand = undefined

-- return a list of Yakus and Fu's of a winning hand
calcOneWinningHand :: WinningHand -> ([Yaku], [Fu])
calcOneWinningHand w = undefined

calcYakuAndFu :: WinningHand -> ([Yaku], [Fu]) -> (Int, Int)
calcYakuAndFu w (yakus, fus) = undefined

getYakus :: WinningHand -> [Yaku]
getYakus w =
  appendYakus isRiichi (Normal Riichi) w
    ++ appendYakus isIppatsu (Normal Ippatsu) w
    ++ appendYakus isClosedTsumo (Normal ClosedTsumo) w
    ++ appendYakus isAllSimple (Normal AllSimple) w
    ++ appendYakus isHonorTiles (honorTilesDYakus) w
  where
    honorTilesDYakus = zipWith (\i t -> Normal HonorTiles t) [1 ..] (getHonorTiles w)

appendYakus :: (WinningHand -> Bool) -> Yaku -> WinningHand -> [Yaku]
appendYakus cond yaku w
  | cond w = [yaku]
  | otherwise = []

-- getFus :: [Fu] -> WinningHand -> [Fu]
-- getFus fs w
--   | isClosedMeld (hand w) && riichiStatus w == SRiichi = fs : Normal Riichi
--   | otherwise = fs

calcYakus :: [Yaku] -> Int
calcYakus yakus
  | any isYakumanYaku yakus = sum (map convertToHans (filter isYakumanYaku yakus))
  | otherwise = sum (map convertToHans yakus)

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
    ThreeMixedSequences -> 2
    ThreeMixedSequencesOpened -> 1
    FullStraight -> 2
    FullStraightOpened -> 1
    CommonEnds -> 2
    CommonEndsOpened -> 1
    DoubleTwinSequences -> 3
    HalfFlush -> 3
    HalfFlushOpened -> 2
    CommonTerminals -> 3
    CommonTerminalsOpened -> 2
    FullFlush -> 6
    FullFlushOpened -> 5
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

scoreTable :: WinningHand -> (Int, Int) -> Int
scoreTable w (han, fu) = undefined
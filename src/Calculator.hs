module Calculator where

import Hand
import Rule
import Tile

type Calculator a = String -> IO a

-- The two metrics for calculating final score
type HanFu = (Int, Int)

data Yaku
  = -- 1 Han yakus
    Riichi
  | Ippatsu
  | ClosedTsumo
  | AllSimple
  | HonorTilesW Wind
  | SelfWindTiles Wind
  | HonorTilesD Dragon
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
  | FullStraight
  | CommonEnds
  | -- 3 Han yakus but 2 if opened
    HalfFlush
  | CommonTerminals
  | DoubleTwinSequences
  | -- 6 Han yakus
    FullFlush
  | -- Forced Mankan
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

calc :: [WinningHand] -> Int
calc hand = undefined

calcOneWinningHand :: WinningHand -> HanFu
calcOneWinningHand w = undefined
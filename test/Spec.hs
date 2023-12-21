{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- module Spec (main) where

-- import Melds

import Data.List (sort)
import Hand
import Meld
import Parser
import Rule
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Exception
import Tile
import Wall

-- import Test.QuickCheck hiding (shuffle)

main :: IO ()
main = hspec do
  context "1.1 Mahjong Tiles" do
    describe "Tiles" do
      it "has value for numeric tiles" do
        evaluate (numeric 2 Sou)
        evaluate (numeric 5 Pin)
        evaluate (numeric 8 Man)
        return ()
      it "has a value for all wind tiles" do
        evaluate (wind East)
        evaluate (wind South)
        evaluate (wind West)
        evaluate (wind North)
        return ()
      it "has a value for dragon tiles" do
        evaluate (dragon White)
        evaluate (dragon Red)
        evaluate (dragon Green)
        return ()
      it "should reject numeric Honor tiles" do
        evaluate (numeric 1 Honor) `shouldThrow` anyException
    describe "Winds" do
      it "should be Honor tiles" do
        property $ forAll anyWind $ \wind -> do
          suitOf wind `shouldBe` Honor
    describe "Dragons" do
      it "should be Honor tiles" do
        property $ forAll anyDragon $ \dragon -> do
          suitOf dragon `shouldBe` Honor

  context "1.2 Melds" do
    describe "sequentialMeld" do
      it "should be in sequence and the same suit" do
        property $ forAll getSequentialMeld $ \m -> do
          case m of
            Triplet t1 t2 t3 opened ->
              sequentialMeld t1 t2 t3 opened `shouldBe` Just (Run t1 t2 t3 opened)
      it "should not allow melds like (9, 1, 2)" do
        property $ forAll anyNumericSuit $ \s -> do
          sequentialMeld (numeric 8 s) (numeric 9 s) (numeric 1 s) False `shouldBe` Nothing
          sequentialMeld (numeric 9 s) (numeric 1 s) (numeric 2 s) False `shouldBe` Nothing
      it "should not allow melds in sequence but not in the same suit" do
        property $ forAll (genNonThreeSame anyNumericSuit) $ \ss -> do
          case ss of
            (s1 : s2 : s3 : _) ->
              sequentialMeld (numeric 3 s1) (numeric 4 s2) (numeric 5 s3) False `shouldBe` Nothing
    describe "tripletMeld" do
      it "should all be the same tiles" do
        property $ forAll getTripletMeld $ \m -> do
          let t1 = numeric 1 Sou
          tripletMeld (numeric 1 Sou) (numeric 2 Sou) (numeric 3 Sou) False `shouldBe` Nothing
          tripletMeld t1 t1 t1 False `shouldBe` Just (Triplet t1 t1 t1 False)

  context "1.3 Wall" do
    describe "fullWall" do
      it "should has length of 136" do
        length fullWall `shouldBe` 136

  context "1.4 Hand" do
    describe "sort Hand" do
      it "should sort a hand in numeric order first, then in suit (s->p->m->z)" do
        let hand = parse "789m1233458p8p333s"
        let sorted = parse "333s12334588p789m"
        sort (fst hand) `shouldBe` fst sorted
        let hand2 = parse "789m88p333s222666z"
        let sorted2 = parse "333s88p789m222666z"
        sort (fst hand2) `shouldBe` fst sorted2
    describe "pluck" do
      it "should only take one tile away" do
        property $ forAll anyTilesOf14 $ \(h : hs) -> do
          pluck h (h : hs) `shouldBe` Just (removeItem h (h : hs))

    describe "getWinHandsByWinTile" do
      it "should return every possible winning hands for 22334455667788m" do
        let h = parse "22334455667788m"
            whs = getWinHandsByWinTile h
        length (uniq $ map hand whs) `shouldBe` 4
      it "should has the last tile as winning hand" do
        let h = parse "12345667778886p"
            whs = getWinHandsByWinTile h
        map winningTile whs
          `shouldMatchList` replicate (length whs) (last (fst h))
        -- with open meld
        let h = parse "12345667776pP888p"
            whs = getWinHandsByWinTile h
        map winningTile whs
          `shouldMatchList` replicate (length whs) (last (fst h))
      it "should find 2 different winning melds for 12345667778886p" do
        let h = parse "12345667776pP888p"
            whs = getWinHandsByWinTile h
        length whs `shouldBe` 2

  context "1.5 Parser" do
    describe "parser" do
      it "should return a valid hand" do
        let s1 = "22233344455566m"
        parse s1 `shouldBe` hand1
        let s2 = "789m1233458p8pP333s"
        parse s2 `shouldBe` hand2
        let s3 = "234p55mC2mP2sK2z"
        parse s3 `shouldBe` hand3

  context "2 Rules" do
    describe "2.1 Han Related" do
      describe "isClosedHand" do
        it "should return True for 22333444555662m" do
          let h = parse "22333444555662m"
              whs = getWinHandsByWinTile h
          map isClosedHand whs `shouldNotContain` [False]
        it "should return False for 2334588p789m1pP333s" do
          let h = parse "2334588p789m1pP333s"
              whs = getWinHandsByWinTile h
          map isClosedHand whs `shouldNotContain` [True]

      describe "isSevenPairs" do
        it "should return True for 33m1155p118s1177z8s" do
          let h = parse "33m1155p118s1177z8s"
              whs = getWinHandsByWinTile h
          map isSevenPairs whs `shouldMatchList` replicate (length whs) True
        it "should return False for 33m1155p118s1277z8s" do
          let h = parse "33m1155p118s1277z8s"
              whs = getWinHandsByWinTile h
          map isSevenPairs whs `shouldMatchList` replicate (length whs) False

      describe "isNoPointsHand" do
        it "should return True for 12355m34789p456s2p" do
          let h = parse "12355m34789p456s2p"
              whs = getWinHandsByWinTile h
          map isNoPointsHand whs `shouldMatchList` replicate (length whs) True
        it "should return False for 12355m34999p456s2p" do
          let h = parse "12355m34999p456s2p"
              whs = getWinHandsByWinTile h
          map isNoPointsHand whs `shouldMatchList` replicate (length whs) False
        it "should return True for opened hand 55m34789p456s2pC1m" do
          let h = parse "55m34789p456s2pC1m"
              whs = getWinHandsByWinTile h
          map isNoPointsHand whs `shouldMatchList` replicate (length whs) True

      describe "isTwinSequences" do
        it "should return True for 24m556677p999s66z3m" do
          let h = parse "24m556677p999s66z3m"
              whs = getWinHandsByWinTile h
          map isTwinSequences whs `shouldMatchList` replicate (length whs) True
        it "should return False for 24m567p999s66z3mC5p" do
          let h = parse "24m567p999s66z3mC5p"
              whs = getWinHandsByWinTile h
          map isTwinSequences whs `shouldMatchList` replicate (length whs) False
        it "should return False for Double Twin Sequence Hands" do
          let h = parse "223344p66778m11z8m"
              whs = getWinHandsByWinTile h
          map isTwinSequences whs `shouldMatchList` replicate (length whs) False

      describe "isDoubleTwinSequences" do
        it "should return True for 223344p66778m11z8m" do
          let h = parse "223344p66778m11z8m"
              whs = getWinHandsByWinTile h
          map isDoubleTwinSequences whs `shouldContain` [True]
        -- The other possibility is Seven Pairs, would return False
        it "should return False for 234p66778m11z8mC2p" do
          let h = parse "234p66778m11z8mC2p"
              whs = getWinHandsByWinTile h
          map isDoubleTwinSequences whs `shouldContain` [False]

      describe "isThreeMixedSequences" do
        it "should return True for 234m111234p234s22z" do
          let h = parse "234m111234p234s22z"
              whs = getWinHandsByWinTile h
          map isThreeMixedSequences whs `shouldMatchList` replicate (length whs) True
        it "should return True for 234m111234p22zC2s" do
          let h = parse "234m111234p22zC2s"
              whs = getWinHandsByWinTile h
          map isThreeMixedSequences whs `shouldMatchList` replicate (length whs) True
        it "should return False for 234m111534p22zC2s" do
          let h = parse "234m111534p22zC2s"
              whs = getWinHandsByWinTile h
          map isThreeMixedSequences whs `shouldMatchList` replicate (length whs) False

      describe "isFullStraight" do
        it "should return True for 999m23456789s55z1s" do
          let h = parse "999m23456789s55z1s"
              whs = getWinHandsByWinTile h
          map isFullStraight whs `shouldMatchList` replicate (length whs) True
        it "should return True for 999m23789s55z1sC456s" do
          let h = parse "999m23456789s55z1s"
              whs = getWinHandsByWinTile h
          map isFullStraight whs `shouldMatchList` replicate (length whs) True

      describe "isAllTripletsYaku" do
        it "should return True for 888m333p11177s44z7s" do
          let h = parse "888m333p11177s44z7s"
              whs = getWinHandsByWinTile h
          map isAllTripletsYaku whs
            `shouldMatchList` replicate (length whs) True
        it "should return True for 888m11177s44z7sK3p" do
          let h = parse "888m11177s44z7sK3p"
              whs = getWinHandsByWinTile h
          map isAllTripletsYaku whs
            `shouldMatchList` replicate (length whs) True
        it "should return False for 888m123p11177s44z7s" do
          let h = parse "888m123p11177s44z7s"
              whs = getWinHandsByWinTile h
          map isAllTripletsYaku whs
            `shouldMatchList` replicate (length whs) False

      describe "isThreeClosedTriplets" do
        it "should return True for 1444m123999p222s1m" do
          let h = parse "1444m123999p222s1m"
              whs = getWinHandsByWinTile h
          map isThreeClosedTriplets whs
            `shouldMatchList` replicate (length whs) True

      describe "isThreeMixedTriplets" do
        it "should return True for 33m333p678p333s33z3m" do
          let h = parse "33m333p678p333s33z3m"
              whs = getWinHandsByWinTile h
          map isThreeMixedTriplets whs
            `shouldMatchList` replicate (length whs) True

      describe "isThreeQuads" do
        it "should return True for 123p22sK8mK1sk4z" do
          let h = parse "123p22sK8mK1sk4z"
              whs = getWinHandsByWinTile h
          map isThreeQuads whs
            `shouldMatchList` replicate (length whs) True

      describe "isAllSimple" do
        it "should return True for 22345678m345666s" do
          let h = parse "22345678m345666s"
              whs = getWinHandsByWinTile h
          map isAllSimple whs `shouldMatchList` replicate (length whs) True
        it "should return False for 11z345678m34566s1z" do
          let h = parse "11z345678m34566s1z"
              whs = getWinHandsByWinTile h
          map isAllSimple whs `shouldMatchList` replicate (length whs) False

      describe "isHonorTiles" do
        it "should return True for 55m123567p888s777z" do
          let h = parse "55m123567p888s777z"
              whs = getWinHandsByWinTile h
          map isHonorTiles whs `shouldMatchList` replicate (length whs) True

      describe "isCommonEnds" do
        it "should return True for 789m11178p999s55z9p" do
          let h = parse "789m11178p999s55z9p"
              whs = getWinHandsByWinTile h
          map isCommonEnds whs `shouldMatchList` replicate (length whs) True

      describe "isCommonTerminals" do
        it "should return True for 123999m99p127893s" do
          let h = parse "123999m99p127893s"
              whs = getWinHandsByWinTile h
          map isCommonTerminals whs `shouldMatchList` replicate (length whs) True

      describe "isAllTerminalsAndHonors" do
        it "should return True for 111m999p11999s66z1s" do
          let h = parse "111m999p11999s66z1s"
              whs = getWinHandsByWinTile h
          map isAllTerminalsAndHonors whs
            `shouldMatchList` replicate (length whs) True
        it "should return True for 11m11p99s12255771z" do
          let h = parse "11m11p99s12255771z"
              whs = getWinHandsByWinTile h
          map isAllTerminalsAndHonors whs
            `shouldMatchList` replicate (length whs) True
        it "should return False for 111m999p22999s666z" do
          let h = parse "111m999p22999s666z"
              whs = getWinHandsByWinTile h
          map isAllTerminalsAndHonors whs
            `shouldMatchList` replicate (length whs) False

      describe "isLittleThreeDragons" do
        it "should return True for 234p678s55666775z" do
          let h = parse "234p678s55666775z"
              whs = getWinHandsByWinTile h
          map isLittleThreeDragons whs `shouldMatchList` replicate (length whs) True

      describe "isHalfFlush" do
        it "should return True for 11134789p22555z5p" do
          let h = parse "11134789p22555z5p"
              whs = getWinHandsByWinTile h
          map isHalfFlush whs `shouldMatchList` replicate (length whs) True
        it "should return False for 11134789p225552p" do
          let h = parse "11134789p225552p"
              whs = getWinHandsByWinTile h
          map isHalfFlush whs `shouldMatchList` replicate (length whs) False
        it "should return False for 11134789s225552s" do
          let h = parse "11134789p225552p"
              whs = getWinHandsByWinTile h
          map isHalfFlush whs `shouldMatchList` replicate (length whs) False

      describe "isFullFlush" do
        it "should return True for 13444556667892s" do
          let h = parse "13444556667892s"
              whs = getWinHandsByWinTile h
          map isFullFlush whs `shouldMatchList` replicate (length whs) True

      describe "isThirteenOrphans" do
        it "should return True for 1m19p19s12234567z9m" do
          let h = parse "1m19p19s12234567z9m"
              whs = getWinHandsByWinTile h
          print whs
          map isThirteenOrphans whs
            `shouldMatchList` replicate (length whs) True
          length whs `shouldNotBe` 0
        it "should return False for 19m19p19s12345672z" do
          let h = parse "19m19p19s12345672z"
              whs = getWinHandsByWinTile h
          map isThirteenOrphans whs
            `shouldMatchList` replicate (length whs) False
          length whs `shouldNotBe` 0

      describe "isThirteenOrphans13Waits" do
        it "should return True for 19m19p19s12345672z" do
          let h = parse "19m19p19s12345672z"
              whs = getWinHandsByWinTile h
          map isThirteenOrphans13Waits whs
            `shouldMatchList` replicate (length whs) True
          length whs `shouldNotBe` 0
        it "should return False for 1m19p19s12234567z9m" do
          let h = parse "1m19p19s12234567z9m"
              whs = getWinHandsByWinTile h
          map isThirteenOrphans13Waits whs
            `shouldMatchList` replicate (length whs) False
          length whs `shouldNotBe` 0

      describe "isFourClosedTriplets" do
        it "should return True for 44488m999p222s11z8m" do
          let h = parse "44488m999p222s11z8m"
              whs' = getWinHandsByWinTile h
              whs = map (\x -> x {isTsumo = True}) whs'
          map isFourClosedTriplets whs
            `shouldMatchList` replicate (length whs) True
        it "should return False if not Tsumo for 555m222999p44466z" do
          let h = parse "555m222999p44466z"
              whs = getWinHandsByWinTile h
          map isFourClosedTriplets whs
            `shouldMatchList` replicate (length whs) False

      describe "isFourClosedTripletsSingleWait" do
        it "should return True for 555m222999p44466z" do
          let h = parse "555m222999p44466z"
              whs = getWinHandsByWinTile h
          map isFourClosedTripletsSingleWait whs
            `shouldMatchList` replicate (length whs) True
        it "should return False for 44488m999p222s11z8m" do
          let h = parse "44488m999p222s11z8m"
              whs = getWinHandsByWinTile h
          map isFourClosedTripletsSingleWait whs
            `shouldMatchList` replicate (length whs) False
        it "should return False even if Tsumo for 44488m999p222s11z8m" do
          let h = parse "44488m999p222s11z8m"
              whs' = getWinHandsByWinTile h
              whs = map (\x -> x {isTsumo = True}) whs'
          map isFourClosedTripletsSingleWait whs
            `shouldMatchList` replicate (length whs) False

      describe "isBigThreeDragons" do
        it "should return True for 567m44s555666777z" do
          let h = parse "567m44s555666777z"
              whs = getWinHandsByWinTile h
          map isBigThreeDragons whs
            `shouldMatchList` replicate (length whs) True

      describe "isLittleFourWinds" do
        it "should return True for 678s11122233443z" do
          let h = parse "678s11122233443z"
              whs = getWinHandsByWinTile h
          map isLittleFourWinds whs
            `shouldMatchList` replicate (length whs) True

      describe "isBigFourWinds" do
        it "should return True for 33p111222333444z" do
          let h = parse "33p111222333444z"
              whs = getWinHandsByWinTile h
          map isBigFourWinds whs
            `shouldMatchList` replicate (length whs) True

      describe "isAllHonors" do
        it "should return True for 11222335556661z" do
          let h = parse "11222335556661z"
              whs = getWinHandsByWinTile h
          map isAllHonors whs
            `shouldMatchList` replicate (length whs) True

      describe "isAllTerminals" do
        it "should return True for 11m111999p11999s1m" do
          let h = parse "11m111999p11999s1m"
              whs = getWinHandsByWinTile h
          map isAllTerminals whs
            `shouldMatchList` replicate (length whs) True

      describe "isAllGreen" do
        it "should return True for 22334466888s66z6s" do
          let h = parse "22334466888s66z6s"
              whs = getWinHandsByWinTile h
          map isAllGreen whs
            `shouldMatchList` replicate (length whs) True

      describe "isNineGates" do
        it "should return True for 11123445679998m" do
          let h = parse "11123445679998m"
              whs = getWinHandsByWinTile h
          map isNineGates whs
            `shouldMatchList` replicate (length whs) True

      describe "isNineGates9Waits" do
        it "should return True for 11123456789991p" do
          let h = parse "11123456789991p"
              whs = getWinHandsByWinTile h
          map isNineGates9Waits whs
            `shouldMatchList` replicate (length whs) True

      describe "isFourQuads" do
        it "should return True for 11pk2sk8mK1sK4z" do
          let h = parse "11pk2sk8mK1sK4z"
              whs = getWinHandsByWinTile h
          map isFourQuads whs
            `shouldMatchList` replicate (length whs) True

    describe "2.2 Fu Related" do
      describe "isTwoSideWait" do
        it "should return True for " do
          pendingWith "put some test here"
        it "should return False for " do
          pendingWith "put some test here"

      describe "isOneSideWait" do
        it "should return True for " do
          pendingWith "put some test here"
        it "should return False for " do
          pendingWith "put some test here"

      describe "isDragonPair" do
        it "should return True for " do
          pendingWith "put some test here"
        it "should return False for " do
          pendingWith "put some test here"

      describe "isWindPair" do
        it "should return True for " do
          pendingWith "put some test here"
        it "should return False for " do
          pendingWith "put some test here"

      describe "isSelfWindPair" do
        it "should return True for " do
          pendingWith "put some test here"
        it "should return False for " do
          pendingWith "put some test here"

instance Arbitrary Suit where
  arbitrary = arbitrary

instance Arbitrary Wind where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary Dragon where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary Tile where
  arbitrary =
    oneof
      [ anyNumericTile,
        wind <$> arbitrary,
        dragon <$> arbitrary
      ]

-- instance Arbitrary Opened where
--   arbitrary = arbitrary

anyTile :: Gen Tile
anyTile = arbitrary

anyNumericTile :: Gen Tile
anyNumericTile = Numeric <$> chooseInt (1, 9) <*> elements [Sou, Pin, Man]

anyNonTerminalTile :: Gen Tile
anyNonTerminalTile = Numeric <$> chooseInt (2, 8) <*> elements [Sou, Pin, Man]

anyTerminalTile :: Gen Tile
anyTerminalTile = Numeric <$> elements [1, 9] <*> elements [Sou, Pin, Man]

any1to7NumericTile :: Gen Tile
any1to7NumericTile = Numeric <$> chooseInt (1, 7) <*> elements [Sou, Pin, Man]

anyWind :: Gen Tile
anyWind = wind <$> arbitrary

anyDragon :: Gen Tile
anyDragon = dragon <$> arbitrary

anyNumericSuit :: Gen Suit
anyNumericSuit = elements [Sou, Pin, Man]

anyTilesOf14 :: Gen [Tile]
anyTilesOf14 = vectorOf 14 (elements fullWall)

getSequentialMeld :: Gen Meld
getSequentialMeld = do
  t1 <- any1to7NumericTile
  t2 <- case nextNumeric t1 of
    Just (Numeric n s) -> pure $ Numeric n s
  t3 <- case nextNumeric t2 of
    Just (Numeric n s) -> pure $ Numeric n s
  opened <- elements [True, False]
  return $ Triplet t1 t2 t3 opened

newtype TripletMeld = TripletMeld Meld
  deriving (Eq, Show)

getTripletMeld :: Gen Meld
getTripletMeld = do
  t1 <- arbitrary
  opened <- elements [True, False]
  return $ Triplet t1 t1 t1 opened

dealWithNumericTile :: Maybe Tile -> Tile
dealWithNumericTile = undefined

fromTripletMeld :: TripletMeld -> Meld
fromTripletMeld (TripletMeld meld) = meld

-- Custom generator for a list with the specified constraint
genNonThreeSame :: (Eq a) => Gen a -> Gen [a]
genNonThreeSame gen = do
  xs <- vectorOf 3 gen
  if isAllSame xs
    then genNonThreeSame gen
    else return xs

genThreeSequential :: Gen [Tile]
genThreeSequential = do
  xs <- vectorOf 3 arbitrary
  if isSequence xs
    then genThreeSequential
    else return xs

-- genHand :: Gen Hand
-- genHand = do
--   tiles <- arbitrary
--   melds <- arbitrary
--   return (tiles, melds)

sevenPairHand :: Hand
sevenPairHand =
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

removeItem :: (Eq a) => a -> [a] -> [a]
removeItem _ [] = []
removeItem x (y : ys)
  | x == y = ys
  | otherwise = y : removeItem x ys

-- 22233344455566m
hand1 :: Hand
hand1 =
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
      Numeric 5 Man,
      Numeric 6 Man,
      Numeric 6 Man
    ],
    []
  )

-- 789m1233458p8p#P333s
hand2 :: Hand
hand2 =
  ( [ Numeric 7 Man,
      Numeric 8 Man,
      Numeric 9 Man,
      Numeric 1 Pin,
      Numeric 2 Pin,
      Numeric 3 Pin,
      Numeric 3 Pin,
      Numeric 4 Pin,
      Numeric 5 Pin,
      Numeric 8 Pin,
      Numeric 8 Pin
    ],
    [Triplet (Numeric 3 Sou) (Numeric 3 Sou) (Numeric 3 Sou) True]
  )

-- 234p55m#C2mP2sK2z
hand3 :: Hand
hand3 =
  ( [ Numeric 2 Pin,
      Numeric 3 Pin,
      Numeric 4 Pin,
      Numeric 5 Man,
      Numeric 5 Man
    ],
    [ Run (Numeric 2 Man) (Numeric 3 Man) (Numeric 4 Man) True,
      Triplet (Numeric 2 Sou) (Numeric 2 Sou) (Numeric 2 Sou) True,
      Quad (Wind South) (Wind South) (Wind South) (Wind South) True
    ]
  )

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
        let hand = parser "789m1233458p8p333s"
        let sorted = parser "333s12334588p789m"
        sort (fst hand) `shouldBe` fst sorted
        let hand2 = parser "789m88p333s222666z"
        let sorted2 = parser "333s88p789m222666z"
        sort (fst hand2) `shouldBe` fst sorted2
    describe "pluck" do
      it "should only take one tile away" do
        property $ forAll anyTilesOf14 $ \(h : hs) -> do
          pluck h (h : hs) `shouldBe` Just (removeItem h (h : hs))

  context "1.5 Parser" do
    describe "parser" do
      it "should return a valid hand" do
        let s1 = "22233344455566m"
        parser s1 `shouldBe` hand1
        let s2 = "789m1233458p8pP333s"
        parser s2 `shouldBe` hand2
        let s3 = "234p55mC2mP2sK2z"
        parser s3 `shouldBe` hand3

  context "2 Rules" do
    describe "2.1 Han Related" do
      describe "isClosedHand" do
        it "should return True for " do
          pendingWith "put some test here"
        it "should return False for " do
          pendingWith "put some test here"

      describe "isAllSimple" do
        it "should return True for " do
          pendingWith "put some test here"
        it "should return False for " do
          pendingWith "put some test here"

      describe "isPinfu" do
        it "should return True for " do
          pendingWith "put some test here"
        it "should return False for " do
          pendingWith "put some test here"

      describe "isTwinSequences" do
        it "should return True for " do
          pendingWith "put some test here"
        it "should return False for " do
          pendingWith "put some test here"

      describe "isDoubleTwinSequences" do
        it "should return True for " do
          pendingWith "put some test here"
        it "should return False for " do
          pendingWith "put some test here"

      describe "isThreeMixedSequences" do
        it "should return True for " do
          pendingWith "put some test here"
        it "should return False for " do
          pendingWith "put some test here"

      describe "isFullStraight" do
        it "should return True for " do
          pendingWith "put some test here"
        it "should return False for " do
          pendingWith "put some test here"

      describe "isAllTriplets" do
        it "should return True for " do
          pendingWith "put some test here"
        it "should return False for " do
          pendingWith "put some test here"

      describe "isHalfFlush" do
        it "should return True for " do
          pendingWith "put some test here"
        it "should return False for " do
          pendingWith "put some test here"

      describe "isFullFlush" do
        it "should return True for " do
          pendingWith "put some test here"
        it "should return False for " do
          pendingWith "put some test here"

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

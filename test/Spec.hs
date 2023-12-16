{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- module Spec (main) where

-- import Melds

import Melds
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Exception
import Tiles

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
      it "should be in sequence" do
        pendingWith "Add actual test here"
      it "should not allow melds like (9, 1, 2)" do
        sequentialMeld (numeric 8 Man) (numeric 9 Man) (numeric 1 Man) False `shouldBe` Nothing
        sequentialMeld (numeric 9 Sou) (numeric 1 Sou) (numeric 2 Sou) False `shouldBe` Nothing
      it "should not allow melds in sequence but not in the same suit" do
        sequentialMeld (numeric 3 Sou) (numeric 4 Man) (numeric 5 Pin) False `shouldBe` Nothing
    describe "tripletMeld" do
      it "should all be the same tiles" do
        property $ forAll getTripletMeld $ \m -> do
          -- let m'@(Triplet t1 t2 t3 _) = fromTripletMeld m
          let t1 = numeric 1 Sou
          tripletMeld (numeric 1 Sou) (numeric 2 Sou) (numeric 3 Sou) False `shouldBe` Nothing
          tripletMeld t1 t1 t1 False `shouldBe` Just (Triplet t1 t1 t1 False)

-- property do
--   return $

-- context "1.3 Walls" do
--   describe "Wall" do
--     it "returns a set of 136 tiles " $
--       property
--         do
--           d <- wall
--           length d `shouldBe` 136
-- context "1.4 Yaku" do
--   describe "All simples" do
--     it "occurs when there's no 1, 9 or Honor tiles" do
--       property do
--         undefined

-- context "1.5 Hand Score" do
--   describe "Winning Hand" do
--     it "should consist of 4 melds and 1 pair" do

instance Arbitrary Suit where
  arbitrary = arbitrary

instance Arbitrary Wind where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary Dragon where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary Tile where
  arbitrary =
    oneof
      [ Numeric <$> arbitrary <*> arbitrary,
        wind <$> arbitrary,
        dragon <$> arbitrary
      ]

-- instance Arbitrary Opened where
--   arbitrary = arbitrary

anyTile :: Gen Tile
anyTile = arbitrary

anyNumericTile :: Gen Tile
anyNumericTile = Numeric <$> arbitrary <*> elements [Sou, Pin, Man]

anyOneToSevenNumericTile :: Gen Tile
anyOneToSevenNumericTile = Numeric <$> chooseInt (1, 7) <*> elements [Sou, Pin, Man]

anyWind :: Gen Tile
anyWind = wind <$> arbitrary

anyDragon :: Gen Tile
anyDragon = dragon <$> arbitrary

getMeld :: Gen Meld
getMeld = undefined

newtype SequentialMeld = SequentialMeld Meld
  deriving (Eq, Show)

instance Arbitrary SequentialMeld where
  arbitrary = do
    t1 <- anyNumericTile
    let t2 = cycleNext t1
    let t3 = cycleNext t2
    opened <- elements [True, False]
    -- case t2 of Nothing -> Numeric
    return (SequentialMeld (Triplet t1 t2 t3 opened))

getSequentialMeld :: Gen SequentialMeld
getSequentialMeld = arbitrary

newtype TripletMeld = TripletMeld Meld
  deriving (Eq, Show)

instance Arbitrary TripletMeld where
  arbitrary = do
    tile <- anyTile
    opened <- elements [True, False]
    return (TripletMeld (Triplet tile tile tile opened))

getTripletMeld :: Gen TripletMeld
getTripletMeld = arbitrary

fromTripletMeld :: TripletMeld -> Meld
fromTripletMeld (TripletMeld meld) = meld
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE InstanceSigs #-}

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
        property do
          wind <- anyWind
          return $ suitOf wind `shouldBe` Honor
    describe "Dragons" do
      it "should be Honor tiles" do
        property do
          dragon <- anyDragon
          return $ suitOf dragon `shouldBe` Honor

  context "1.2 Melds" do
    describe "sequenceMeld" do
      it "should be in sequence" do
        pendingWith "Add actual test here"
      it "should not allow melds like (9, 1, 2)" do
        sequenceMeld (numeric 8 Man) (numeric 9 Man) (numeric 1 Man) False `shouldBe` Nothing
        sequenceMeld (numeric 9 Sou) (numeric 1 Sou) (numeric 2 Sou) False `shouldBe` Nothing
      it "should not allow melds in sequence but not in the same suit" do
        sequenceMeld (numeric 3 Sou) (numeric 4 Man) (numeric 5 Pin) False `shouldBe` Nothing
    describe "tripletMeld" do
      it "should all be the same tiles" do
        tripletMeld (numeric 3 Pin) (numeric 3 Pin) (numeric 5 Pin) False `shouldBe` Nothing
        tripletMeld (numeric 1 Sou) (numeric 1 Sou) (numeric 1 Sou) False `shouldBe` Just (triplet (numeric 1 Sou) (numeric 1 Sou) (numeric 1 Sou) False)

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

-- newtype HonorTile =

anyTile :: Gen Tile
anyTile = arbitrary

anyWind :: Gen Tile
anyWind = wind <$> arbitrary

anyDragon :: Gen Tile
anyDragon = dragon <$> arbitrary
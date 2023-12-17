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
              sequentialMeld t1 t2 t3 opened `shouldBe` Just (Triplet t1 t2 t3 opened)
      it "should not allow melds like (9, 1, 2)" do
        property $ forAll anyNumericSuit $ \s -> do
          sequentialMeld (numeric 8 s) (numeric 9 s) (numeric 1 s) False `shouldBe` Nothing
          sequentialMeld (numeric 9 s) (numeric 1 s) (numeric 2 s) False `shouldBe` Nothing
      it "should not allow melds in sequence but not in the same suit" do
        property $ do
          ss <- generate $ resize 10 $ genNonThreeSame anyNumericSuit
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
      describe "sortHand" do
        it "should sort a hand in numeric order than in suit" do
          pendingWith "pending..."

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

newtype SequentialMeld = SequentialMeld Meld
  deriving (Eq, Show)

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

newtype SortedHand = SortedHand Hand

-- Custom generator for a list with the specified constraint
genNonThreeSame :: (Eq a) => Gen a -> Gen [a]
genNonThreeSame genElement = do
  elements <- listOf genElement
  return $ filterConsecutive3 elements

-- Helper function to filter out lists with three consecutive identical elements
filterConsecutive3 :: (Eq a) => [a] -> [a]
filterConsecutive3 [] = []
filterConsecutive3 [x] = [x]
filterConsecutive3 [x, y] = [x, y]
filterConsecutive3 (x : y : z : rest)
  | x == y && y == z = filterConsecutive3 (x : y : rest)
  | otherwise = x : filterConsecutive3 (y : z : rest)
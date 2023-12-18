{-# LANGUAGE InstanceSigs #-}

module Wall where

import Data.List
import System.Random
import Tiles

type Hand = [Tile]

type Wall = [Tile]

handToWall :: Hand -> Wall
handToWall h = h

data Indexed i a = Index {index :: i, item :: a}
  deriving (Show, Eq, Ord)

at :: a -> i -> Indexed i a
a `at` i = Index i a

shuffle :: (Ord a) => [Int] -> [a] -> [a]
shuffle ixs as = shuffled
  where
    shuffled = case unzip (sort (zip ixs as)) of
      (ixs, as) -> as

fullWall :: Wall
fullWall = [Numeric n s | s <- [Sou, Pin, Man], n <- [1 .. 9], _ <- [1 .. 4]] ++ [wind w | w <- winds, _ <- [1 .. 4]] ++ [dragon d | d <- dragons, _ <- [1 .. 4]]

copyNTimes :: Int -> a -> [a]
copyNTimes x y = map (const y) [1 .. x]

shuffledWall :: IO Wall
shuffledWall = do
  seed <- newStdGen
  let rand = randoms seed
  return (shuffle rand fullWall)

sortHand :: Hand -> Hand
sortHand = sort

instance Ord Tile where
  compare :: Tile -> Tile -> Ordering
  compare (Numeric n1 s1) (Numeric n2 s2)
    | s1 == s2 && n1 < n2 = LT
    | s1 == s2 && n1 == n2 = EQ
    | s1 == s2 && n1 > n2 = GT
    | s1 < s2 = LT
    | otherwise = GT
  compare (Wind w1 s1) (Wind w2 s2) = compare (w1, s1) (w2, s2)
  compare (Dragon d1 s1) (Dragon d2 s2) = compare (d1, s1) (d2, s2)
  compare (Numeric _ _) (Wind _ _) = LT
  compare (Wind _ _) (Numeric _ _) = GT
  compare (Numeric _ _) (Dragon _ _) = LT
  compare (Dragon _ _) (Numeric _ _) = GT
  compare (Wind _ _) (Dragon _ _) = LT
  compare (Dragon _ _) (Wind _ _) = GT

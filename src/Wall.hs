{-# LANGUAGE InstanceSigs #-}

module Wall where

import Data.List
import System.Random
import Tile

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

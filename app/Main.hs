module Main where

import Calculator
import Data.List
import Data.Maybe
import Hand
import Meld
import Parser
import Rule
import Tile
import Wall

main :: IO ()
main = do
  -- -- let h = parse "33p111222z333s444z"
  -- let h = parse "2233445667788p"
  -- putStr $ show "You entered:"
  -- print $ h
  -- let whs = getWinHandsByWinTile h
  --     w = head whs
  -- if length whs < 1
  --   then do
  --     print "No valid winning hand available. Check your input"
  --   else do
  --     print $ "The hand can be split into the following melds:"
  --     print (map hand whs)
  --     putStrLn $ unlines (map show (sortBy (flip compare) (calc whs)))
  mainLoop newCalculator
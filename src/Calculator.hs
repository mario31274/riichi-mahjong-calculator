module Calculator where

import Hand
import Rule

type Calculator a = String -> IO a

calc :: Hand -> HanFu
calc hand = undefined
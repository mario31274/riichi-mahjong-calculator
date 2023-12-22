module Calculator where

import Data.Char
import Hand
import Meld (Meld (Single))
import Parser
import Player
import Rule
import Score
import System.Exit
import Text.Read (readMaybe)
import Tile
import Wall

type Prompt a = String -> IO a

data Option = StartCalculator | StartGame

data Move = Discard [Int]
  deriving (Show, Eq)

-- openPosition = 34*(3-(d+2)%4)+d*2
data Board = Board
  { player1 :: Player,
    player2 :: Player,
    player3 :: Player,
    player4 :: Player,
    wall :: Wall,
    round :: Wind,
    player1Wind :: Wind,
    counterSticks :: Int
  }
  deriving (Show)

data Calculator a = Calculator
  { toBeCalc :: [WinningHand],
    results :: [Result],
    ask :: a
  }

sanitizeAnswer :: Prompt String -> Prompt String
sanitizeAnswer prompt question = do
  ans <- prompt question
  return (trimLeadingSpaces (trimTrailingSpaces ans))

trimLeadingSpaces :: String -> String
trimLeadingSpaces s
  | null s = s
  | not (isSpace c) = s
  | otherwise = trimLeadingSpaces (tail s)
  where
    c = head s

seekNonSpace :: String -> Maybe (String, Char, String)
seekNonSpace s
  | null s = Nothing
  | not (isSpace c) = Just ("", c, tail s)
  | otherwise =
      case result of
        Just (w, c', r) -> Just (c : w, c', r)
        Nothing -> Nothing
  where
    c = head s
    result = seekNonSpace (tail s)

trimTrailingSpaces :: String -> String
trimTrailingSpaces s
  | null s = s
  | otherwise =
      case seekNonSpace s of
        Just (w, c, r) -> w ++ [c] ++ trimTrailingSpaces r
        Nothing -> ""

sanitizeInputForTiles :: String -> String
sanitizeInputForTiles = filter (`elem` ['1' .. '9'] ++ ['s', 'p', 'm', 'z', 'C', 'P', 'K', 'k'])

parseOption :: String -> Maybe Option
parseOption s
  | s == "1" = Just StartCalculator
  | s == "2" = Just StartGame
  | otherwise = Nothing

parseHand :: String -> Maybe Hand
parseHand s
  | isValidHand (parse s) = Just (parse s)
  | otherwise = Nothing

parseWind :: String -> Maybe Wind
parseWind s = case head [toLower c | c <- s] of
  'e' -> Just East
  's' -> Just South
  'w' -> Just West
  'n' -> Just North
  _ -> Nothing

parseYesNo :: String -> Maybe Bool
parseYesNo s = case head [toLower c | c <- s] of
  'y' -> Just True
  'n' -> Just False
  _ -> Nothing

parseRiichi :: String -> Maybe Riichi
parseRiichi s = case head [toLower c | c <- s] of
  's' -> Just SRiichi
  'd' -> Just DbRiichi
  _ -> Just NoRiichi

parseInt :: String -> Maybe Int
parseInt s = case readMaybe s of
  Nothing -> Nothing
  Just n -> if n < 0 then Nothing else Just n

parseBonusAgari :: String -> Maybe BonusAgari
parseBonusAgari s = case head s of
  '1' -> Just DeadWallDrawAgari
  '2' -> Just RobbingAQuadAgari
  '3' -> Just UnderTheSeaAgari
  '4' -> Just UnderTheRiverAgari
  '5' -> Just NagashiManganAgari
  '6' -> Just BlessingOfHeavenAgari
  '7' -> Just BlessingOfEarthAgari
  '8' -> Just BlessingOfManAgari
  _ -> Just NoBonus

askHand :: Calculator (Prompt Hand) -> IO Hand
askHand calculator = do
  let query = "Enter the hand you want to calculate:"
  ask calculator query

askRoundWind :: Calculator (Prompt Wind) -> IO Wind
askRoundWind calculator = do
  ask calculator "What's the Round Wind? (e/s/w/n)"

askSelfWind :: Calculator (Prompt Wind) -> IO Wind
askSelfWind calculator = do
  ask calculator "What's the Seat Wind? (e/s/w/n)"

askTsumo :: Calculator (Prompt Bool) -> IO Bool
askTsumo calculator = do
  ask calculator "Is this a Tsumo hand (self-picked hand)? (y/n)"

askDora :: Calculator (Prompt Int) -> IO Int
askDora calculator = do
  ask calculator "How many Doras (Bonus tiles) are in this hand?"

askRiichi :: Calculator (Prompt Riichi) -> IO Riichi
askRiichi calculator = do
  ask calculator "Is Riichi? (s = Single Riichi, d = Double Riichi)?"

askIppatsu :: Calculator (Prompt Bool) -> IO Bool
askIppatsu calculator = do
  ask calculator "Is Ippatsu (one-shot)? (y/n)"

askBonusAgari :: Calculator (Prompt BonusAgari) -> IO BonusAgari
askBonusAgari calculator = do
  let query =
        "Is there one of the following special win:\n"
          ++ "0 = None of the below\n"
          ++ "1 = DeadWallDraw\n"
          ++ "2 = RobbingAQuad\n"
          ++ "3 = UnderTheSea\n"
          ++ "4 = UnderTheRiver\n"
          ++ "5 = NagashiMangan\n"
          ++ "6 = BlessingOfHeaven\n"
          ++ "7 = BlessingOfEarth\n"
          ++ "8 = BlessingOfMan\n"
  ask calculator query

helpAdvice :: Prompt String -> String -> Prompt String
helpAdvice prompt advice question = do
  answer <- prompt question
  case answer of
    "help" -> do
      putStrLn advice
      helpAdvice prompt advice question
    other ->
      return answer

simplePrompt :: Prompt String
simplePrompt question = do
  putStrLn question
  getLine

untilQuit :: Prompt String -> Prompt String
untilQuit prompt question = do
  quitOrAnswer <- prompt question
  case quitOrAnswer of
    "quit" -> exitSuccess
    answer -> return answer

retryPrompt :: Prompt (Maybe a) -> Prompt a
retryPrompt prompt question = do
  answer <- prompt question
  case answer of
    Nothing -> do
      retryPrompt prompt question
    Just answer ->
      return answer

retryPromptHand :: Prompt (Maybe Hand) -> Prompt Hand
retryPromptHand prompt question = do
  answer <- prompt question
  case answer of
    Nothing -> do
      let errorMessage = "Invalid Hand. Please Check your input.\n"
      retryPrompt prompt (errorMessage ++ question)
    Just answer ->
      return answer

changeAnswerBy :: Prompt a -> (a -> b) -> Prompt b
changeAnswerBy prompt change question = do
  answer <- prompt question
  return (change answer)

parseAnswerAs :: Prompt a -> (a -> Maybe b) -> Prompt b
parseAnswerAs prompt parse = retryPrompt (prompt `changeAnswerBy` parse)

parseAnswerAsHand :: Prompt a -> (a -> Maybe Hand) -> Prompt Hand
parseAnswerAsHand prompt parse = retryPromptHand (prompt `changeAnswerBy` parse)

-- ask Hand
promptHand :: Prompt Hand
promptHand = untilQuit simplePrompt `parseAnswerAsHand` parseHand

-- ask Tsumo
promptTsumo :: Prompt Bool
promptTsumo = untilQuit simplePrompt `parseAnswerAs` parseYesNo

-- ask Round Wind
promptRoundWind :: Prompt Wind
promptRoundWind = untilQuit simplePrompt `parseAnswerAs` parseWind

-- ask Self Wind
promptSelfWind :: Prompt Wind
promptSelfWind = untilQuit simplePrompt `parseAnswerAs` parseWind

-- ask Dora
promptDora :: Prompt Int
promptDora = untilQuit simplePrompt `parseAnswerAs` parseInt

-- ask Riichi
promptRiichi :: Prompt Riichi
promptRiichi = untilQuit simplePrompt `parseAnswerAs` parseRiichi

-- ask Ippatsu
promptIppatsu :: Prompt Bool
promptIppatsu = untilQuit simplePrompt `parseAnswerAs` parseYesNo

-- ask BonusAgari
promptBonusAgari :: Prompt BonusAgari
promptBonusAgari = untilQuit simplePrompt `parseAnswerAs` parseBonusAgari

newCalculator :: Calculator (Prompt Hand)
newCalculator = Calculator {toBeCalc = [], results = [], ask = promptHand}

inputHand :: Calculator (Prompt Hand) -> IO (Calculator (Prompt Wind))
inputHand calculator = do
  h <- askHand calculator
  let whs = getWinHandsByDefault h
  return calculator {toBeCalc = whs, ask = promptRoundWind}

inputRoundWind :: Calculator (Prompt Wind) -> IO (Calculator (Prompt Wind))
inputRoundWind = inputWind askRoundWind promptSelfWind

inputSelfWind :: Calculator (Prompt Wind) -> IO (Calculator (Prompt Int))
inputSelfWind = inputWind askSelfWind promptDora

inputWind :: (Calculator (Prompt Wind) -> IO Wind) -> Prompt a -> Calculator (Prompt Wind) -> IO (Calculator (Prompt a))
inputWind this next calculator = do
  wind <- this calculator
  let whs' = map (\w -> w {roundWind = wind}) (toBeCalc calculator)
  return calculator {toBeCalc = whs', ask = next}

inputDora :: Calculator (Prompt Int) -> IO (Calculator (Prompt Riichi))
inputDora calculator = do
  dora <- askDora calculator
  let whs' = map (\w -> w {dora = dora}) (toBeCalc calculator)
  return calculator {toBeCalc = whs', ask = promptRiichi}

mainLoop :: Calculator (Prompt Hand) -> IO a
mainLoop calculator = do
  calc' <-
    inputRoundWind
      =<< inputHand calculator
  print $ toBeCalc calc'
  let results = calc (toBeCalc calc')
  print $ results
  mainLoop newCalculator

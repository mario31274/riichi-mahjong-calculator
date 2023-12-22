module Calculator where

-- import Control.Lens
import Data.Char
import Data.List (sort)
import Hand
import Meld
import Parser
import Player
import Rule
import Score
import System.Exit
import Text.Read (readMaybe)
import Tile
import Wall

data Calculator a = Calculator
  { handInput :: Hand,
    toBeCalc :: [WinningHand],
    results :: [Result],
    ask :: a
  }

type Prompt a = String -> IO a

data Option = StartCalculator | StartGame

data Move = Discard [Int]
  deriving (Show, Eq)

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
parseWind (c : _)
  | isAlpha c =
      case toLower c of
        'e' -> Just East
        's' -> Just South
        'w' -> Just West
        'n' -> Just North
        _ -> Nothing
  | otherwise = Nothing
parseWind [] = Nothing

parseYesNo :: String -> Maybe Bool
parseYesNo (c : _)
  | isAlpha c =
      case toLower c of
        'y' -> Just True
        'n' -> Just False
        _ -> Nothing
  | otherwise = Nothing
parseYesNo [] = Nothing

parseRiichi :: String -> Maybe Riichi
parseRiichi (c : _)
  | isAlpha c =
      case toLower c of
        'y' -> Just SRiichi
        'd' -> Just DbRiichi
        _ -> Just NoRiichi
  | otherwise = Just NoRiichi
parseRiichi [] = Just NoRiichi

parseInt :: String -> Maybe Int
parseInt s = case readMaybe s of
  Nothing -> Nothing
  Just n -> if n < 0 then Nothing else Just n

parseBonusAgari :: String -> Maybe BonusAgari
parseBonusAgari (c : _)
  | isDigit c =
      case toLower c of
        '1' -> Just DeadWallDrawAgari
        '2' -> Just RobbingAQuadAgari
        '3' -> Just UnderTheSeaAgari
        '4' -> Just UnderTheRiverAgari
        '5' -> Just NagashiManganAgari
        '6' -> Just BlessingOfHeavenAgari
        '7' -> Just BlessingOfEarthAgari
        '8' -> Just BlessingOfManAgari
        _ -> Just NoBonus
  | otherwise = Just NoBonus
parseBonusAgari [] = Just NoBonus

askHand :: Calculator (Prompt Hand) -> IO Hand
askHand calculator =
  do
    let query =
          "Enter the hand you want to calculate:\n"
            ++ "  1　2  3  4　5  6  7　8  9   Cns = Chi (Run) meld\n"
            ++ "s 🀐　🀑　🀒　🀓　🀔　🀕　🀖　🀗　🀘   Pns = Pon (Triplet) meld\n"
            ++ "p 🀙　🀚　🀛　🀜　🀝　🀞　🀟　🀠　🀡   Kns = Open Kan (Quad) meld\n"
            ++ "m 🀇　🀈　🀉　🀊　🀋　🀌　🀍　🀎　🀏   kns = Closed Kan meld\n"
            ++ "z 　 🀀　🀁　🀂　🀃　🀆　🀅 🀄      * Must put the melds behind all tiles\n"
            ++ "     1　2  3  4  5  6  7"
    -- ++ "  1 2 3 4 5 6 7 8 9\n"
    -- ++ "s 🀐 🀑 🀒 🀓 🀔 🀕 🀖 🀗 🀘\n"
    -- ++ "p 🀙 🀚 🀛 🀜 🀝 🀞 🀟 🀠 🀡\n"
    -- ++ "m 🀇 🀈 🀉 🀊 🀋 🀌 🀍 🀎 🀏\n"
    -- ++ "z   🀀 🀁 🀂 🀃 🀄 🀅 🀆\n"
    -- ++ "    1 2 3 4 5 6 7"
    ask calculator query

askRoundWind :: Calculator (Prompt Wind) -> IO Wind
askRoundWind calculator = do
  ask calculator "What's the Round Wind? (e/s/w/n)"

askSelfWind :: Calculator (Prompt Wind) -> IO Wind
askSelfWind calculator = do
  ask calculator "What's the Seat Wind? (e/s/w/n)"

askTsumo :: Calculator (Prompt Bool) -> IO Bool
askTsumo calculator = do
  ask calculator "Is this a Tsumo hand (self-draw tile)? (y/n)"

askDora :: Calculator (Prompt Int) -> IO Int
askDora calculator = do
  ask calculator "How many Doras (Bonus tiles) are in this hand?"

askRiichi :: Calculator (Prompt Riichi) -> IO Riichi
askRiichi calculator = do
  ask calculator "Riichi hand? (y = Yes, Single Riichi, d = Double Riichi, Default=No)"

askIppatsu :: Calculator (Prompt Bool) -> IO Bool
askIppatsu calculator = do
  ask calculator "Is Ippatsu (one-shot)? (y/n)"

askBonusAgari :: Calculator (Prompt BonusAgari) -> IO BonusAgari
askBonusAgari calculator = do
  let query =
        "Is there one of the following rare conditions? (Default = 0)\n"
          ++ "0 = None of the below\n"
          ++ "1 = DeadWallDraw  5 = NagashiMangan\n"
          ++ "2 = RobbingAQuad  6 = BlessingOfHeave\n"
          ++ "3 = UnderTheSea   7 = BlessingOfEarth\n"
          ++ "4 = UnderTheRiver 8 = BlessingOfMan"
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
    "quit" -> do
      putStrLn "Goodbye."
      exitSuccess
    answer -> return answer

retryPrompt :: Prompt (Maybe a) -> Prompt a
retryPrompt prompt question = do
  answer <- prompt question
  case answer of
    Nothing -> do
      retryPrompt prompt question
    Just answer ->
      return answer

retryWithErrorMessage :: Prompt (Maybe a) -> String -> Prompt a
retryWithErrorMessage prompt errorMessage question = do
  answer <- prompt question
  case answer of
    Nothing -> do
      retryPrompt prompt (errorMessage ++ question)
    Just answer ->
      return answer

changeAnswerBy :: Prompt a -> (a -> b) -> Prompt b
changeAnswerBy prompt change question = do
  answer <- prompt question
  return (change answer)

parseAnswerAs :: Prompt a -> (a -> Maybe b) -> Prompt b
parseAnswerAs prompt parse = retryPrompt (prompt `changeAnswerBy` parse)

parseAnswerAsHand :: Prompt a -> (a -> Maybe b) -> Prompt b
parseAnswerAsHand prompt parse =
  retryWithErrorMessage (prompt `changeAnswerBy` parse) msg
  where
    msg = "Invalid Hand. Please Check your input.\n"

parseAnswerWithMsg :: Prompt a -> (a -> Maybe b) -> Prompt b
parseAnswerWithMsg prompt parse =
  retryWithErrorMessage (prompt `changeAnswerBy` parse) msg
  where
    msg = "Sorry, I don't understand that.\n"

-- ask Hand
promptHand :: Prompt Hand
promptHand = untilQuit simplePrompt `parseAnswerAsHand` parseHand

-- ask Tsumo
promptTsumo :: Prompt Bool
promptTsumo = untilQuit simplePrompt `parseAnswerAs` parseYesNo

-- ask Round Wind
promptRoundWind :: Prompt Wind
promptRoundWind = untilQuit simplePrompt `parseAnswerWithMsg` parseWind

-- ask Self Wind
promptSelfWind :: Prompt Wind
promptSelfWind = untilQuit simplePrompt `parseAnswerWithMsg` parseWind

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
newCalculator = Calculator {handInput = ([], []), toBeCalc = [], results = [], ask = promptHand}

inputHand :: Calculator (Prompt Hand) -> IO (Calculator (Prompt Wind))
inputHand calculator = do
  h <- askHand calculator
  let whs = getWinHandsByDefault h
  return calculator {handInput = h, toBeCalc = whs, ask = promptRoundWind}

inputRoundWind :: Calculator (Prompt Wind) -> IO (Calculator (Prompt Wind))
-- inputRoundWind = inputWind askRoundWind promptSelfWind
inputRoundWind calculator = do
  wind <- askRoundWind calculator
  let whs' = map (\w -> w {roundWind = wind}) (toBeCalc calculator)
  return calculator {toBeCalc = whs', ask = promptSelfWind}

inputSelfWind :: Calculator (Prompt Wind) -> IO (Calculator (Prompt Bool))
-- inputSelfWind = inputWind askSelfWind promptTsumo
inputSelfWind calculator = do
  wind <- askSelfWind calculator
  let whs' = map (\w -> w {selfWind = wind}) (toBeCalc calculator)
  return calculator {toBeCalc = whs', ask = promptTsumo}

-- inputWind :: (Calculator (Prompt Wind) -> IO Wind) -> Prompt a -> Calculator (Prompt Wind) -> IO (Calculator (Prompt a))
-- inputWind this next calculator = do
--   wind <- this calculator
--   let whs' = map (\w -> w {roundWind = wind}) (toBeCalc calculator)
--   return calculator {toBeCalc = whs', ask = next}

inputTsumo :: Calculator (Prompt Bool) -> IO (Calculator (Prompt Int))
inputTsumo calculator = do
  tsumo <- askTsumo calculator
  let whs' = map (\w -> w {isTsumo = tsumo}) (toBeCalc calculator)
  return calculator {toBeCalc = whs', ask = promptDora}

inputDora :: Calculator (Prompt Int) -> IO (Calculator (Prompt Riichi))
inputDora calculator = do
  dora <- askDora calculator
  let whs' = map (\w -> w {dora = dora}) (toBeCalc calculator)
  return calculator {toBeCalc = whs', ask = promptRiichi}

inputRiichi :: Calculator (Prompt Riichi) -> IO (Calculator (Prompt Bool))
inputRiichi calculator = do
  riichi <- askRiichi calculator
  let whs' = map (\w -> w {riichiStatus = riichi}) (toBeCalc calculator)
  return calculator {toBeCalc = whs', ask = promptIppatsu}

inputIppatsu :: Calculator (Prompt Bool) -> IO (Calculator (Prompt BonusAgari))
inputIppatsu calculator = do
  ippatsu <- askIppatsu calculator
  let whs' = map (\w -> w {isIppatsu = ippatsu}) (toBeCalc calculator)
  return calculator {toBeCalc = whs', ask = promptBonusAgari}

changePrompt :: Prompt b -> Calculator (Prompt a) -> Calculator (Prompt b)
changePrompt newPrompt calculator = calculator {ask = newPrompt}

inputBonusAgari :: Calculator (Prompt BonusAgari) -> IO (Calculator (Prompt BonusAgari))
inputBonusAgari calculator = do
  bonus <- askBonusAgari calculator
  let whs' = map (\w -> w {bonusAragi = bonus}) (toBeCalc calculator)
  return calculator {toBeCalc = whs'}

-- inputAtoB :: (Calculator (Prompt a) -> IO a) -> Prompt b -> Calculator (Prompt a) -> IO (Calculator (Prompt b))
-- inputAtoB this next calculator = do
--   answer <- this calculator
--   let whs' = map (\w -> w {XXX = bonus}) (toBeCalc calculator)
--   return calculator {toBeCalc = whs', ask = next}

mainLoop :: Calculator (Prompt Hand) -> IO a
mainLoop calculator = do
  calc1 <-
    inputHand calculator
      >>= inputRoundWind
      >>= inputSelfWind
      >>= inputTsumo
      >>= inputDora
      >>= inputRiichi
  calc2 <-
    case riichiStatus (head (toBeCalc calc1)) of
      NoRiichi -> do
        inputBonusAgari (changePrompt promptBonusAgari calc1)
      _ -> do
        inputIppatsu calc1 >>= inputBonusAgari
  putStrLn "You Entered:"
  print $ handInput calc2
  putStr $ unlines (map show (sort (calc (toBeCalc calc2))))
  untilQuit simplePrompt "Press Enter to calculate next hand."
  mainLoop newCalculator

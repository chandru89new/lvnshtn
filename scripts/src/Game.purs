module Game where

import Prelude

import Control.Alternative (guard)
import Control.Apply (lift2)
import Control.Monad.State (state)
import Data.Array (elem, (!!), (..))
import Data.Array as A
import Data.Foldable (sequence_, sum, traverse_)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String (Pattern(..), joinWith, length, split, trim)
import Data.Traversable (for, sequence, traverse, traverse_)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Effect.Random (randomInt)
import Node.Process (exit, exit')
import Node.ReadLine (close, createConsoleInterface, noCompletion)
import Node.ReadLine.Aff (question)
import Words (words3, words4, words5)

type Reason = String

data GameAction
  = StartGame
  | ChooseDifficulty
  | SetDifficulty Int
  | AskUserInput
  | UserPlayed String
  | AskComputerToPlay
  | UserForfeited
  | ComputerPlayed String
  | ComputerForfeited
  | UserWon
  | ComputerWon

data Player = You | Computer

type GameState =
  { lastPlayedWord :: String
  , dictionary :: Set String
  , playedPath :: Array String
  , gameWords :: Tuple String String
  , currentPlayer :: Player
  }

runGame :: GameAction -> GameState -> Aff Unit
runGame StartGame state = do
  log introText
  runGame ChooseDifficulty state
runGame ChooseDifficulty state = do
  input <- readLine $ colorInfo "Choose word length (3, 4, or 5)"
  if (elem input [ "3", "4", "5" ]) then do
    runGame (SetDifficulty (fromMaybe 3 (fromString input))) state
  else do
    log $ colorError "Invalid input. Please enter 3, 4, or 5."
    runGame ChooseDifficulty state
runGame (SetDifficulty wordLength) state = do
  let
    dict = getAllWordsByLen wordLength
    -- wrds <- getRandomPlayableWord dict
    wrds = Tuple "code" "vats"
  let
    newPath = [ fst wrds ]
    newState = (state { lastPlayedWord = fst wrds, gameWords = wrds, currentPlayer = You, dictionary = dict, playedPath = newPath })
  log $ showPath newState
  runGame AskUserInput newState
runGame AskUserInput state = do
  input <- readLine $ colorUser "You (or hit Enter to forfeit)"
  runGame (UserPlayed input) state
runGame (UserPlayed word) state = do
  if trim word == "" then
    runGame UserForfeited state
  else do
    let allowed = rankByClosest (snd state.gameWords) $ (A.filter (\w -> not $ elem w state.playedPath)) $ getAllPossibilities state.dictionary state.lastPlayedWord
    -- check if word is already played
    when (elem word state.playedPath) $ do
      log $ colorError "Word already played. Try again."
      log $ showPath state
      runGame AskUserInput state
    -- check if word is in dictionary
    when (not $ isValidWord state.dictionary word) do
      log $ colorError "Word not in dictionary. Try again."
      log $ showPath state
      runGame AskUserInput state
    -- check if word is one character distance only
    when (not $ elem word allowed) do
      log $ colorError "Word must differ by exactly one letter. Try again."
      log $ showPath state
      runGame AskUserInput state
    -- check if word is the target word
    if word == snd state.gameWords then
      runGame UserWon state
    else do
      let newPath = state.playedPath <> [ word ]
      let newState = state { lastPlayedWord = word, playedPath = newPath, currentPlayer = Computer }
      log $ showPath newState
      runGame AskComputerToPlay newState
runGame AskComputerToPlay state = do
  let allowed = rankByClosest (snd state.gameWords) $ (A.filter (\w -> not $ elem w state.playedPath)) $ getAllPossibilities state.dictionary state.lastPlayedWord
  if A.length allowed == 0 then
    runGame ComputerForfeited state
  else do
    let
      path = map snd $ getShortestPath state.dictionary state.lastPlayedWord (snd state.gameWords)
      nextBestWord = do
        p <- path
        case p !! 1 of
          Just w
            | elem w state.playedPath -> A.head allowed
            | otherwise -> Just w
          Nothing -> A.head allowed
    case nextBestWord of
      Nothing -> runGame ComputerForfeited state
      Just word -> runGame (ComputerPlayed word) state
runGame (ComputerPlayed word) state = do
  log $ colorWarning $ "Computer played: " <> word
  if word == snd state.gameWords then
    runGame ComputerWon state
  else do
    let newPath = state.playedPath <> [ word ]
    let newState = state { lastPlayedWord = word, playedPath = newPath, currentPlayer = You }
    log $ showPath newState
    runGame AskUserInput newState
runGame UserWon _ = do
  log $ colorSuccess "You win!"
  liftEffect $ exit
runGame ComputerWon _ = do
  log $ colorWarning "Computer wins!"
  liftEffect $ exit
runGame UserForfeited _ = do
  log $ colorError "You forfeited! Computer wins!"
  liftEffect $ exit
runGame ComputerForfeited _ = do
  log $ "Computer can't think of any word!"
  log $ colorSuccess "You win!"
  liftEffect $ exit

readLine :: String -> Aff String
readLine str = do
  interface <- liftEffect $ createConsoleInterface noCompletion
  input <- question (str <> ": ") interface
  _ <- liftEffect $ close interface
  pure input

data Color = Red | Green | Blue | Yellow

colorLog :: Color -> String -> String
colorLog Red str = "\x1b[31m" <> str <> "\x1b[0m"
colorLog Green str = "\x1b[32m" <> str <> "\x1b[0m"
colorLog Blue str = "\x1b[34m" <> str <> "\x1b[0m"
colorLog Yellow str = "\x1b[33m" <> str <> "\x1b[0m"

colorError :: String -> String
colorError = colorLog Red

colorWarning :: String -> String
colorWarning = colorLog Yellow

colorSuccess :: String -> String
colorSuccess = colorLog Green

colorInfo :: String -> String
colorInfo = colorLog Blue

colorUser :: String -> String
colorUser = colorLog Blue

introText :: String
introText =
  colorSuccess "\nWelcome to wordladder!\n"
    <> colorInfo "You"
    <> " vs. "
    <> colorWarning "Computer\n"
    <> "Change one letter at a time to get a new word.\n"
    <> "Reach the goal word first and win!\n"
    <> "Let's begin!\n"

getAllWordsByLen :: Int -> Set String
getAllWordsByLen n =
  if n == 3 then words3 else if n == 4 then words4 else words5

getRandomPlayableWord :: forall m. MonadEffect m => Set String -> m (Tuple String String)
getRandomPlayableWord dict = do
  source <- liftEffect $ getRandomWord (Set.toUnfoldable dict)
  target <- liftEffect $ getRandomWord (Set.toUnfoldable dict)
  shortestPath <- pure $ join $ lift2 (\s t -> map snd (getShortestPath dict s t)) source target
  case shortestPath of
    Nothing -> getRandomPlayableWord dict
    Just _ -> pure $ Tuple (fromMaybe "" source) (fromMaybe "" target)

randomNum :: Int -> Effect Int
randomNum max = randomInt 0 max

getRandomWord :: Array String -> Effect (Maybe String)
getRandomWord dictionary = do
  idx <- randomNum (A.length dictionary - 1)
  pure $ dictionary !! idx

getShortestPath :: Set String -> String -> String -> Maybe (Tuple Int (Array String))
getShortestPath dictionary source target =
  if length source /= length target then
    Nothing
  else
    let
      res = walk dictionary target [ [ source ] ] (Set.singleton source)
    in
      map (\path -> Tuple (A.length path - 1) path) res

walk :: Set String -> String -> Array (Array String) -> Set String -> Maybe (Array String)
walk dictionary target queue visited =
  case dequeue queue of
    Tuple Nothing _ -> Nothing
    Tuple (Just path) remainingQ ->
      let
        lastWord = A.last path
      in
        case lastWord of
          Nothing -> walk dictionary target remainingQ visited
          Just wrd ->
            if wrd == target then
              Just path
            else
              let
                newPossibilities = A.filter (\w -> not (Set.member w visited))
                  (getAllPossibilities dictionary wrd)
                newVisited = Set.union visited (Set.fromFoldable newPossibilities)
                newPaths = map (\w -> path <> [ w ]) newPossibilities
                newQueue = remainingQ <> newPaths
              in
                walk dictionary target newQueue newVisited

dequeue :: forall a. Array a -> Tuple (Maybe a) (Array a)
dequeue queue = Tuple (A.head queue) (fromMaybe [] $ A.tail queue)

getAllPossibilities :: Set String -> String -> Array String
getAllPossibilities dictionary wrd =
  let
    alphabets = split (Pattern "") "abcdefghijklmnopqrstuvwxyz"
    wrdAsArray = split (Pattern "") wrd
    indices = 0 .. (length wrd - 1)
    possibilities = do
      idx <- indices
      c <- alphabets
      let newWord = A.mapWithIndex (\i ch -> if i == idx then c else ch) wrdAsArray
      guard (isValidWord dictionary (joinWith "" newWord) && (joinWith "" newWord) /= wrd)
      pure (joinWith "" newWord)
  in
    possibilities

isValidWord :: Set String -> String -> Boolean
isValidWord dictionary wrd = Set.member wrd dictionary

rankByClosest :: String -> Array String -> Array String
rankByClosest target wrds = A.sortBy (comparing (hammingDistance target)) wrds

hammingDistance :: String -> String -> Int
hammingDistance wrd1 wrd2 =
  let
    chars1 = split (Pattern "") wrd1
    chars2 = split (Pattern "") wrd2
    differences = A.zipWith (\c1 c2 -> if c1 == c2 then 0 else 1) chars1 chars2
  in
    sum differences

showPath :: GameState -> String
showPath state =
  let
    t = snd state.gameWords
    playedWords = A.mapWithIndex (\i w -> if i == 0 then w else if i > 0 && mod i 2 == 0 then colorWarning w else colorUser w) state.playedPath
  in
    (joinWith " → " playedWords) <> " ... " <> t

data GameEffect
  = Log String
  | Exit Int
  | AskInput String (String -> Game)

type Game = { state :: GameState, effects :: (Array GameEffect), action :: GameAction }

handleEffect :: GameEffect -> Aff Unit
handleEffect (Log msg) = log msg
handleEffect (Exit code) = liftEffect $ exit' code
handleEffect (AskInput prompt k) = do
  input <- readLine prompt
  _ <- pure $ k input
  pure unit

handleEffects :: Array GameEffect -> Aff Unit
handleEffects effects = traverse_ handleEffect effects

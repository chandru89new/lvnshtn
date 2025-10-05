module Main where

import Prelude

import Control.Alternative (guard)
import Control.Apply (lift2)
import Data.Array (elem, (!!), (..))
import Data.Array as A
import Data.Foldable (sum)
import Data.Int (fromString, toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String (Pattern(..), joinWith, length, split, trim)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay, launchAff_)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Effect.Random (randomInt)
import Node.Process (exit')
import Node.ReadLine (close, createConsoleInterface, noCompletion)
import Node.ReadLine.Aff (question)
import Words (words3, words4, words5)

-- MAIN
main :: Effect Unit
main = do
  log introText
  let
    initialState =
      { lastPlayedWord: ""
      , dictionary: Set.empty
      , playedPath: []
      , gameWords: Tuple "" ""
      , currentState: NotInitialized
      }
  launchAff_ $ do
    let
      loop state = do
        let Tuple newState effects = updateGameState state
        if A.length effects == 0 then
          pure unit
        else do
          finalState <- handleEffects newState effects
          loop finalState
    loop initialState

-- TYPES
data GameEffect
  = Log String
  | Exit Int
  | AskUserToChooseDifficulty
  | InitializeGame
  | AskUserToPlay
  | AskComputerToPlay

data CurrentState
  = NotInitialized
  | DifficultySet Int
  | UserPlayed String
  | ComputerPlayed String

data Color = Red | Green | Blue | Yellow

type GameState =
  { lastPlayedWord :: String
  , dictionary :: Set String
  , playedPath :: Array String
  , gameWords :: Tuple String String
  , currentState :: CurrentState
  }

-- FUNCTIONS
updateGameState :: GameState -> Tuple GameState (Array GameEffect)
updateGameState state = case state.currentState of
  NotInitialized -> Tuple state [ AskUserToChooseDifficulty ]
  DifficultySet int -> Tuple (state { dictionary = dict }) [ InitializeGame ]
    where
    dict = getAllWordsByLen int

  UserPlayed word -> result
    where
    result =
      let
        isValidEnglishWord = isValidWord state.dictionary word
        isInAllowed = elem word (getAllPossibilities state.dictionary state.lastPlayedWord)
        isAlreadyPlayed = elem word state.playedPath
        hasWon = word == snd state.gameWords
        newPath = state.playedPath <> [ word ]
      in
        if trim word == "" then (Tuple state [ Log (colorError "You forfeited! Computer wins!"), Exit 0 ])
        else if isAlreadyPlayed then (Tuple state [ Log (colorError "Word already played. Try again."), Log (showPath state), AskUserToPlay ])
        else if not isValidEnglishWord then (Tuple state [ Log (colorError "Word not in dictionary. Try again."), Log (showPath state), AskUserToPlay ])
        else if not isInAllowed then (Tuple state [ Log (colorError "Word must differ by exactly one letter. Try again."), Log (showPath state) ])
        else if hasWon then (Tuple state [ Log (colorSuccess "You win!"), Exit 0 ])
        else
          let
            newState = state { lastPlayedWord = word, playedPath = newPath }
          in
            (Tuple newState [ Log (showPath newState), Log "Thinking...", AskComputerToPlay ])
  ComputerPlayed word -> result
    where
    result =
      let
        hasWon = word == snd state.gameWords
        newPath = state.playedPath <> [ word ]
      in
        if hasWon then (Tuple state [ Log (colorWarning $ "Computer plays '" <> word <> "' and wins!"), Exit 0 ])
        else
          let
            newState = state { lastPlayedWord = word, playedPath = newPath }
          in
            (Tuple newState [ Log (colorWarning $ "Computer played: " <> word), Log (showPath newState), AskUserToPlay ])

handleEffect :: GameState -> GameEffect -> Aff (Tuple GameState (Array GameEffect))
handleEffect state (Log msg) = do
  log msg
  pure $ Tuple state []
handleEffect _ (Exit code) = do
  liftEffect $ exit' code
handleEffect state AskUserToChooseDifficulty = do
  input <- readLine $ colorInfo "Choose word length (3, 4, or 5)"
  if (elem input [ "3", "4", "5" ]) then
    pure $ Tuple (state { currentState = DifficultySet (fromMaybe 3 (fromString input)) }) []
  else do
    log $ colorError "Invalid input. Please enter 3, 4, or 5."
    pure $ Tuple state [ AskUserToChooseDifficulty ]
handleEffect state InitializeGame = do
  let dict = state.dictionary
  wrds <- liftEffect $ getRandomPlayableWord dict
  let
    newPath = [ fst wrds ]
    newState = (state { lastPlayedWord = fst wrds, gameWords = wrds, playedPath = newPath })
  pure $ Tuple newState [ Log $ showPath newState, AskUserToPlay ]
handleEffect state AskUserToPlay = do
  input <- readLine $ colorUser "You (or hit Enter to forfeit)"
  pure $ Tuple (state { currentState = UserPlayed input }) []
handleEffect state AskComputerToPlay = do
  _ <- delay (Milliseconds (toNumber 1000))
  let allowed = rankByClosest (snd state.gameWords) $ (A.filter (\w -> not $ elem w state.playedPath)) $ getAllPossibilities state.dictionary state.lastPlayedWord
  if A.length allowed == 0 then
    pure $ Tuple state [ Log "Computer can't think of any word!", Log (colorSuccess "You win!"), Exit 0 ]
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
      Nothing -> pure $ Tuple state [ Log "Computer can't think of any word!", Log (colorSuccess "You win!"), Exit 0 ]
      Just word -> pure $ Tuple (state { currentState = ComputerPlayed word }) []

handleEffects :: GameState -> Array GameEffect -> Aff GameState
handleEffects state effects = go effects state
  where
  go :: Array GameEffect -> GameState -> Aff GameState
  go effs s = case A.head effs of
    Nothing -> pure $ s
    Just e -> do
      Tuple s' newEffs <- handleEffect s e
      go (newEffs <> (fromMaybe [] $ A.tail effs)) s'

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

readLine :: String -> Aff String
readLine str = do
  interface <- liftEffect $ createConsoleInterface noCompletion
  input <- question (str <> ": ") interface
  _ <- liftEffect $ close interface
  pure input

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

-- CONSTANTS

introText :: String
introText =
  colorSuccess "\nWelcome to wordladder!\n"
    <> colorInfo "You"
    <> " vs. "
    <> colorWarning "Computer\n"
    <> "Change one letter at a time to get a new word.\n"
    <> "Reach the goal word first and win!\n"
    <> "Let's begin!\n"

-- INSTANCES

instance showCurrentState :: Show CurrentState where
  show NotInitialized = "NotInitialized"
  show (DifficultySet n) = "DifficultySet " <> show n
  show (UserPlayed str) = "UserPlayed " <> str
  show (ComputerPlayed str) = "ComputerPlayed " <> str

instance showGameEffect :: Show GameEffect where
  show (Log str) = "Log " <> str
  show (Exit n) = "Exit " <> show n
  show AskUserToChooseDifficulty = "AskUserToChooseDifficulty"
  show InitializeGame = "InitializeGame"
  show AskUserToPlay = "AskUserToPlay"
  show AskComputerToPlay = "AskComputerToPlay"

derive instance Eq CurrentState
derive instance Eq GameEffect
derive instance Eq Color


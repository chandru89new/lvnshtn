module Main where

import Prelude

import Control.Alternative (guard)
import Control.Apply (lift2)
import Data.Array (elem, filter, head, last, mapWithIndex, null, sortBy, tail, zipWith, (!!), (..))
import Data.Array (length) as A
import Data.Foldable (sum)
import Data.Int (fromNumber, toNumber)
import Data.Map (Map, fromFoldable, lookup) as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (abs)
import Data.Set (Set)
import Data.Set as Set
import Data.String (Pattern(..), joinWith, length, split, trim)
import Data.String.Utils (lines)
import Data.Tuple (Tuple(..), snd)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import Effect.Random (randomInt)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, writeTextFile)
import Node.Process (exit)
import Node.ReadLine (close, createConsoleInterface, noCompletion)
import Node.ReadLine.Aff (question)

main :: Effect Unit
main = launchAff_ $ do
  s <- startGame
  gameLoop s

writeDict :: Effect Unit
writeDict = do
  words <- getAllWords
  writeTextFile UTF8 "filtered_words.txt" (joinWith "," $ Set.toUnfoldable words)

getAllWords :: Effect (Set String)
getAllWords = do
  dump <- readTextFile UTF8 "words_alpha.txt"
  pure $ Set.fromFoldable (filter filterWord (lines dump))

getAllWordsByLen :: Int -> Effect (Set String)
getAllWordsByLen n = do
  dump <- readTextFile UTF8 "words_alpha.txt"
  pure $ Set.fromFoldable (filter (\w -> length w == n) (lines dump))

filterWord :: String -> Boolean
filterWord str = length str >= 3 && length str <= 5

isReachable :: Set String -> String -> String -> Boolean
isReachable dictionary wrd1 wrd2 =
  length wrd1 == length wrd2 && canReach dictionary wrd1 wrd2

canReach :: Set String -> String -> String -> Boolean
canReach dictionary source target =
  if source == target then true
  else go [ source ] (Set.empty)
  where
  go :: Array String -> Set String -> Boolean
  go wordsToCheck visited =
    if elem target wordsToCheck then
      true
    else
      case head wordsToCheck of
        Nothing -> false
        Just wrd ->
          let
            newSet = Set.insert wrd visited
            possibilities = getAllPossibilities dictionary wrd
            newWordsToCheck = removeDupes $ filter (\w -> not (Set.member w newSet)) possibilities <> (fromMaybe [] (tail wordsToCheck))
          in
            go (newWordsToCheck) newSet

removeDupes :: Array String -> Array String
removeDupes = Set.toUnfoldable <<< Set.fromFoldable

getAllPossibilities :: Set String -> String -> Array String
getAllPossibilities dictionary wrd =
  let
    alphabets = split (Pattern "") "abcdefghijklmnopqrstuvwxyz"
    wrdAsArray = split (Pattern "") wrd
    indices = 0 .. (length wrd - 1)
    possibilities = do
      idx <- indices
      c <- alphabets
      let newWord = mapWithIndex (\i ch -> if i == idx then c else ch) wrdAsArray
      guard (isValidWord dictionary (joinWith "" newWord) && (joinWith "" newWord) /= wrd)
      pure (joinWith "" newWord)
  in
    possibilities

isValidWord :: Set String -> String -> Boolean
isValidWord dictionary wrd = Set.member wrd dictionary

randomNum :: Int -> Effect Int
randomNum max = randomInt 0 max

getRandomWord :: Array String -> Effect String
getRandomWord dictionary = do
  idx <- randomNum (A.length dictionary - 1)
  pure $ fromMaybe "" (dictionary !! idx)

-- main :: Effect Unit
-- main = do
--   dict <- getAllWords
--   source <- getRandomWord (Set.toUnfoldable dict)
--   target <- getRandomWord (Set.toUnfoldable dict)
--   log $ "Source: " <> show source
--   log $ "Target: " <> show target
--   -- log $ "Can be played? " <> (show $ isReachable dict source target)
--   if length source /= length target then log "Words must be of same length"
--   else log $ "Paths: " <> show (getShortestPath dict source target)

-- getPath :: Set String -> String -> String -> Array String
-- getPath dictionary source target =

enqueue :: forall a. a -> Array a -> Array a
enqueue item queue = [ item ] <> queue

dequeue :: forall a. Array a -> Tuple (Maybe a) (Array a)
dequeue queue = Tuple (head queue) (fromMaybe [] $ tail queue)

walk :: Set String -> String -> Array (Array String) -> Set String -> Maybe (Array String)
walk dictionary target queue visited =
  case dequeue queue of
    Tuple Nothing _ -> Nothing
    Tuple (Just path) remainingQ ->
      let
        lastWord = last path
      in
        case lastWord of
          Nothing -> walk dictionary target remainingQ visited
          Just wrd ->
            if wrd == target then
              Just path
            else
              let
                newPossibilities = filter (\w -> not (Set.member w visited))
                  (getAllPossibilities dictionary wrd)
                newVisited = Set.union visited (Set.fromFoldable newPossibilities)
                newPaths = map (\w -> path <> [ w ]) newPossibilities
                newQueue = remainingQ <> newPaths
              in
                walk dictionary target newQueue newVisited

getShortestPath :: Set String -> String -> String -> Maybe (Tuple Int (Array String))
getShortestPath dictionary source target =
  if length source /= length target then
    Nothing
  else
    let
      res = walk dictionary target [ [ source ] ] (Set.singleton source)
    in
      map (\path -> Tuple (A.length path - 1) path) res

test :: String -> String -> Effect Unit
test s t = do
  dict <- getAllWords
  if not (isValidWord dict s) then log (s <> " is not a valid word")
  else if not (isValidWord dict t) then log (t <> " is not a valid word")
  else if length s /= length t then log "Words must be of same length"
  else logShow $ map snd $ getShortestPath dict s t

type GameState =
  { gameStatus :: GameStatus
  , dictionary :: Set String
  , gameWords :: Tuple String String
  , lastPlayedWord :: String
  , gameType :: GameType
  , playedWords :: Array String
  }

data GameType = PvC | PvP

data GameStatus = Play Player | Win Player | Over String
data Player = User | Computer

instance showPlayer :: Show Player where
  show User = "User"
  show Computer = "Computer"

instance showGameStatus :: Show GameStatus where
  show (Play player) = "Play: " <> show player
  show (Win player) = "Winner: " <> show player
  show (Over reason) = "Game Over: " <> reason

instance showGameType :: Show GameType where
  show PvC = "Player vs Computer"
  show PvP = "Player vs Player"

gameLoop :: GameState -> Aff Unit
gameLoop state = do
  case state.gameStatus of
    Win player -> do
      case player of
        User -> logA "User wins!"
        Computer -> logA "Computer wins!"
      liftEffect $ exit
    Play player -> do
      logA $ showPath state
      allowed <- pure $ rankByClosest (snd state.gameWords) $ (filter (\w -> not $ elem w state.playedWords)) $ getAllPossibilities state.dictionary state.lastPlayedWord
      when (null allowed) $ do
        gameLoop (state { gameStatus = Over "No more possible words can be played. Game ends without a winner!" })
      case player of
        User -> do
          input <- readLine "Enter your word (enter empty to forfeit)"
          when (trim input == "") $ do
            logA "You forfeited the game."
            gameLoop (state { gameStatus = Win Computer })
          when (elem input state.playedWords) $ do
            logA $ "That's already played, friend. Try another."
            gameLoop state
          when (not (isValidWord state.dictionary input)) do
            logA "Hey, that's not a valid word in my dictionary. Try again."
            gameLoop state
          when (not $ elem input allowed) $ do
            logA $ "Only one letter change at a time, friend. Try again."
            gameLoop state
          gameLoop
            ( state
                { gameStatus =
                    if input == snd state.gameWords then Win User
                    else Play Computer
                , lastPlayedWord = input
                , playedWords = state.playedWords <> [ input ]
                }

            )
        Computer -> do
          if (elem (snd state.gameWords) allowed) then do
            logA $ "\nComputer plays: " <> (snd state.gameWords) <> " and wins!"
            liftEffect $ exit
          else
            let
              path = map snd $ getShortestPath state.dictionary state.lastPlayedWord (snd state.gameWords)
              nextBestWord = join $ map
                ( \p -> case p !! 1 of
                    Just w -> if elem w state.playedWords then (head allowed) else Just w
                    Nothing -> head allowed
                )
                path
            in
              case nextBestWord of
                Nothing -> do
                  logA $ "Computer cant think of a word to play."
                  gameLoop (state { gameStatus = Win User })
                Just p -> do
                  logA $ "\nComputer plays: " <> p
                  gameLoop
                    ( state
                        { gameStatus =
                            if p == snd state.gameWords then Win Computer
                            else Play User
                        , lastPlayedWord = p
                        , playedWords = state.playedWords <> [ p ]
                        }
                    )
    Over reason -> do
      logA $ reason
      liftEffect $ exit

showPath :: GameState -> String
showPath state =
  let
    t = snd state.gameWords
    playedWords = state.playedWords
  in
    (joinWith " -> " playedWords) <> " ... " <> t

gameStateInit :: GameState
gameStateInit =
  { gameStatus: Play User
  , dictionary: Set.empty
  , gameWords: Tuple "" ""
  , lastPlayedWord: ""
  , gameType: PvC
  , playedWords: []
  }

readLine :: String -> Aff String
readLine str = do
  interface <- liftEffect $ createConsoleInterface noCompletion
  input <- question (str <> ": ") interface
  _ <- liftEffect $ close interface
  pure input

startGame :: Aff GameState
startGame = do
  dict <- liftEffect $ getAllWordsByLen 4
  -- source <- liftEffect $ getRandomWord (Set.toUnfoldable dict)
  -- target <- liftEffect $ getRandomWord (Set.toUnfoldable dict)
  let
    source = "doge"
    target = "coin"
  pure $ gameStateInit { dictionary = dict, gameWords = Tuple source target, lastPlayedWord = source, playedWords = [ source ] }

logA :: String -> Aff Unit
logA = liftEffect <<< log

rankByClosest :: String -> Array String -> Array String
rankByClosest target wrds = sortBy (\a b -> compare (scoreWords a target) (scoreWords b target)) wrds

compareChar :: String -> String -> Int
compareChar c1 c2 =
  let
    scoreC1 = Map.lookup c1 wordVal
    scoreC2 = Map.lookup c2 wordVal

    score :: Maybe Int
    score = join $ map fromNumber $ lift2 (\sc1 sc2 -> abs $ toNumber (sc1 - sc2)) scoreC1 scoreC2
  in
    fromMaybe 50 score

scoreWords :: String -> String -> Int
scoreWords wrd1 wrd2 =
  let
    chars1 = split (Pattern "") wrd1
    chars2 = split (Pattern "") wrd2
    pairs = zipWith compareChar chars1 chars2
  in
    sum pairs

wordVal :: Map.Map String Int
wordVal =
  let
    alphabets = split (Pattern "") "abcdefghijklmnopqrstuvwxyz"
  in
    Map.fromFoldable $ mapWithIndex (\i c -> Tuple c (i + 1)) alphabets
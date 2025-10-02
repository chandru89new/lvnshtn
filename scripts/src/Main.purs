module Main where

import Prelude

import Control.Alternative (guard)
import Data.Array (elem, filter, head, last, mapWithIndex, null, sortBy, tail, zipWith, (!!), (..))
import Data.Array (length) as A
import Data.Foldable (sum)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String (Pattern(..), joinWith, length, split, trim)
import Data.Tuple (Tuple(..), snd)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import Effect.Random (randomInt)
import Node.Process (exit)
import Node.ReadLine (close, createConsoleInterface, noCompletion)
import Node.ReadLine.Aff (question)
import Words (words4, words3, words5)

main :: Effect Unit
main = launchAff_ $ do
  mainLoop
  where
  mainLoop = do
    input <- readLine "Choose word length (3, 4 or 5)"
    when (not (elem input [ "3", "4", "5" ])) $ do
      logAff "Invalid input. Please enter 3, 4 or 5."
      mainLoop
    s <- startGame (fromMaybe 3 (fromString input))
    gameLoop s

getAllWords :: Set String
getAllWords = Set.unions [ words3, words4, words5 ]

getAllWordsByLen :: Int -> Set String
getAllWordsByLen n =
  if n == 3 then words3 else if n == 4 then words4 else words5

filterWord :: String -> Boolean
filterWord str = length str >= 3 && length str <= 5

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

runGetShortestPath :: String -> String -> Effect Unit
runGetShortestPath s t = do
  let dict = getAllWords
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
      logAff $ (show player) <> " wins!\n"
      liftEffect $ exit
    Play player -> do
      logAff $ showPath state
      allowed <- pure $ rankByClosest (snd state.gameWords) $ (filter (\w -> not $ elem w state.playedWords)) $ getAllPossibilities state.dictionary state.lastPlayedWord
      -- when (null allowed) $ do
      --   gameLoop (state { gameStatus = Over "No more possible words can be played. Game ends without a winner!" })
      case player of
        User -> do
          input <- readLine "Enter your word (enter empty to forfeit)"
          when (trim input == "") $ do
            logAff "You forfeited the game.\n"
            gameLoop (state { gameStatus = Win Computer })
          when (elem input state.playedWords) $ do
            logAff $ "That's already played, friend. Try another.\n"
            gameLoop state
          when (not (isValidWord state.dictionary input)) do
            logAff "Hey, that's not a valid word in my dictionary. Try again.\n"
            gameLoop state
          when (not $ elem input allowed) $ do
            logAff $ "Only one letter change at a time, friend. Try again.\n"
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
            logAff $ "Computer plays: " <> (snd state.gameWords) <> " and wins!\n"
            liftEffect $ exit
          else
            let
              path = map snd $ getShortestPath state.dictionary state.lastPlayedWord (snd state.gameWords)
              nextBestWord = do
                p <- path
                case p !! 1 of
                  Just w
                    | elem w state.playedWords -> head allowed
                    | otherwise -> Just w
                  Nothing -> head allowed
            in
              case nextBestWord of
                Nothing -> do
                  logAff $ "Computer cant think of a word to play.\n"
                  gameLoop (state { gameStatus = Win User })
                Just p -> do
                  logAff $ "Computer plays: " <> p <> "\n"
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
      logAff $ reason
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

startGame :: Int -> Aff GameState
startGame level = do
  let dict = getAllWordsByLen level
  source <- liftEffect $ getRandomWord (Set.toUnfoldable dict)
  target <- liftEffect $ getRandomWord (Set.toUnfoldable dict)
  pure $ gameStateInit { dictionary = dict, gameWords = Tuple source target, lastPlayedWord = source, playedWords = [ source ] }

logAff :: String -> Aff Unit
logAff = liftEffect <<< log

rankByClosest :: String -> Array String -> Array String
rankByClosest target wrds = sortBy (comparing (hammingDistance target)) wrds

hammingDistance :: String -> String -> Int
hammingDistance wrd1 wrd2 =
  let
    chars1 = split (Pattern "") wrd1
    chars2 = split (Pattern "") wrd2
    differences = zipWith (\c1 c2 -> if c1 == c2 then 0 else 1) chars1 chars2
  in
    sum differences

runGetAllPossibilities :: String -> Array String
runGetAllPossibilities word =
  let
    dict = getAllWordsByLen (length word)
  in
    getAllPossibilities dict word
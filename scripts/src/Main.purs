module Main where

import Prelude

import Control.Monad.Writer (runWriter)
import Data.Array (slice)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.String (joinWith, length)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import Game (CurrentState(..), colorError, colorSuccess, getAllWordsByLen, getShortestPath, handleEffects, introText, isValidWord, updateGameState)
import Node.Process (argv)

-- MAIN
main :: Effect Unit
main = do
  args <- do
    a <- argv
    pure $ slice 2 (Array.length a) a
  wrd1 <- pure $ Array.index args 0 
  wrd2 <- pure $ Array.index args 1
  case [wrd1, wrd2] of
    [Nothing, Nothing] -> do
      log introText
      let
        initialState =
          { lastPlayedWord: ""
          , dictionary: Set.empty
          , playedPath: []
          , gameWords: Tuple "" ""
          , currentState: NotInitialized
          , wordLength: 0
          , tries: 0
          }
      launchAff_ $ do
        let
          loop state = do
            let Tuple newState effects = runWriter (updateGameState state)
            finalState <- handleEffects newState effects
            loop finalState
        loop initialState
    [Just w1, Just w2] -> do
      let res = getShortestPath' w1 w2
      log $ case res of
            Left err -> colorError err
            Right path -> 
              if (Array.length path == 0) 
                then colorError "I couldn't find a way to go from " <> w1 <> colorError " to " <> w2 <> "."
                else colorSuccess $ joinWith " → " path  
    _ -> do
      log "I don't recognize that extra command/parameter. To play the game, just `npx wordladder` or `node <path-to-wordladder-file>."
    

getShortestPath' :: String -> String -> Either String (Array String)
getShortestPath' w1 w2 = do
  if (length w1 /= length w2) then (Left "Start and end word should be of the same length.")
  else if (not (isValidWord dict w1 && isValidWord dict w2)) then (Left "Hmm, one of those words is not in my dictionary. Typo?")
  else do
    Right 
    $ case (getShortestPath dict w1 w2) of
        Nothing -> []
        Just (Tuple _ p) -> p
  where
    dict = getAllWordsByLen (length w1)
module Main where

import Prelude
import Game (CurrentState(..), handleEffects, introText, updateGameState)

import Data.Array as A
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)

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
      , wordLength: 0
      }
  launchAff_ $ do
    let
      loop state = do
        let Tuple newState effects = updateGameState state
        if A.length effects == 0 then
          loop newState
        else do
          finalState <- handleEffects newState effects
          loop finalState
    loop initialState

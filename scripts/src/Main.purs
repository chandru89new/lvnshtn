module Main where

import Prelude

import Data.Set as Set
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Game (GameAction(..), Player(..), runGame)

main :: Effect Unit
main = launchAff_ $ do
  runGame StartGame
    { lastPlayedWord: ""
    , dictionary: Set.empty
    , playedPath: [ "" ]
    , gameWords: Tuple "" ""
    , currentPlayer: You
    }
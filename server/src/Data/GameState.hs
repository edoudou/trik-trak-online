module Data.GameState where

import qualified Data.Set as S

import           Types

data GameState = GameState
  { _turn    :: PlayerTurn
  , _over    :: Bool
  , _players :: S.Set Player
  }
  deriving (Eq, Show)

emptyGameState :: GameState
emptyGameState = GameState 0 False S.empty

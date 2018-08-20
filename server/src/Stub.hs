module Stub where

import qualified Data.Map  as M

import           Data.Game

filteredPegs :: M.Map PlayerId [Peg]
filteredPegs = M.fromList
  [ (P1, [PegHome, PegHome, PegTarget (TargetPosition 1), PegBoard (BoardPosition 1) Stake])
  , (P2, [PegHome, PegHome, PegHome, PegHome])
  , (P3, [PegHome, PegHome, PegHome, PegHome])
  , (P4, [PegHome, PegHome, PegHome, PegHome])
  ]

filteredCards :: M.Map PlayerId [Visibility Card]
filteredCards = M.fromList
  [ (P1, setVisible <$> [One, Two, One, Switch])
  , (P2, setHidden <$> [One, Two, One, Switch])
  , (P3, setHidden <$> [One, Two, One, Switch])
  , (P4, setHidden <$> [One, Two, One, Switch])
  ]

actions :: [PlayerAction]
actions = [QuitGame]

history :: [Action]
history = []

filteredGameState :: FilteredGameState
filteredGameState = FilteredGameState
  { _fteams = (Team P1 P3, Team P2 P4)
  , _fcards = filteredCards
  , _fpegs = filteredPegs
  , _fmode = Play P1
  , _factions = actions
  , _fhistory = history
  }

module Stub where

import           Data.Game

filteredPegs :: [(PlayerId, [Peg])]
filteredPegs =
  [ (P1, [PegHome, PegHome, PegTarget (TargetPosition 1), PegBoard (BoardPosition 1) Stake])
  , (P2, [PegHome, PegHome, PegHome, PegHome])
  , (P3, [PegHome, PegHome, PegHome, PegHome])
  , (P4, [PegHome, PegHome, PegHome, PegHome])
  ]

filteredCards :: [(PlayerId, [Visibility Card])]
filteredCards =
  [ (P1, setVisible <$> [One, Two, One, Switch])
  , (P2, setHidden <$> [One, Two, One, Switch])
  , (P3, setHidden <$> [One, Two, One, Switch])
  , (P4, setHidden <$> [One, Two, One, Switch])
  ]

actions :: [PlayerAction]
actions = [QuitGame]

history :: [PlayerAction]
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

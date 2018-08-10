module Stub where

import qualified Data.Map as M

import Data.Game

filteredPegs :: M.Map PlayerId [Peg]
filteredPegs = M.fromList
  [ (P1, [PegHome, PegHome, PegTarget (Position 1), PegBoard (Position 1) Stake])
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

filteredGameState :: FilteredGameState
filteredGameState = FilteredGameState
  { _fturn = P1
  , _fcards = filteredCards
  , _fpegs = filteredPegs
  , _fmode = JoinWait
  }

module Game where

import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Trans.State (get, modify')
import qualified Data.Set                  as S
import           Data.UUID                 (UUID)


import           Data.GameState
import           GameMonad
import           Types

isOver :: GameState -> Bool
isOver _ = undefined

play :: Player -> Card -> Action -> AppMonad ()
play _ _ _ = AppMonad $ do
    _ <- get
    return ()

join :: AppMonad (Either GameError (UUID, PlayerTurn))
join = AppMonad $ do
  gameState <- get
  if (length (_players gameState)) >= 4
     then return (Left JoinTooManyPlayers)
     else do
      let playerTurn = nextPlayerTurn (_players gameState)
      player <- liftIO $ mkPlayer playerTurn emptyHand defaultPegs
      modify' (addPlayer player)
      return (Right (_uuid player, _id player))

  where
    addPlayer :: Player -> GameState -> GameState
    addPlayer player s = s { _players = S.insert player (_players s) }

module Game where

import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Trans.State (get, modify')
import qualified Data.Set                  as S
import           Data.UUID                 (UUID)


import           GameMonad
import           Data.Game

isOver :: GameState -> Bool
isOver _ = undefined

play :: Player -> Card -> Action -> AppMonad ()
play _ _ _ = AppMonad $ do
    _ <- get
    return ()

join :: AppMonad (Either GameError (UUID, PlayerId))
join = AppMonad $ do
  gameState <- get
  case nextPlayerId (_players gameState) of
    Nothing -> return $ Left JoinTooManyPlayers
    Just playerId -> do
      player <- liftIO $ mkPlayer playerId emptyHand defaultPegs
      modify' (addPlayer player)
      return (Right (_uuid player, _id player))

  where
    addPlayer :: Player -> GameState -> GameState
    addPlayer player s = s { _players = S.insert player (_players s) }

module Game where

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Except (ExceptT, runExceptT)
import           Control.Monad.Trans.State  (StateT, get, runStateT)
import           Data.IORef                 (IORef, newIORef)
import qualified Data.Set                   as S
import           Data.UUID                  (UUID)

import           Data.GameState

data GameError
  = InvalidAction Player Action
  deriving (Eq, Show)

type App = StateT GameState (ExceptT GameError IO)

-- TODO: should I use IORef instead of pure state?
runApp :: App a -> GameState -> IO (Either GameError (a, GameState))
runApp action state =
    runExceptT $ runStateT action state

mkEmptyGameStateRef :: IORef GameState
mkEmptyGameStateRef = undefined

isOver :: GameState -> Bool
isOver gameState = undefined

play :: Player -> Card -> Action -> App ()
play player card action = do
    gameState <- get
    return ()

join :: App (UUID, PlayerTurn)
join = do
  gameState <- get
  let playerTurn = nextPlayerTurn (_players gameState)
  player <- liftIO $ mkPlayer playerTurn emptyHand defaultPegs
  return (_uuid player, _id player)

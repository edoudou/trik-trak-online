module Server.Client
  ( clientHealth
  , clientState
  , clientJoin
  , clientPlay
  )
  where

import qualified Data.Text      as T
import           Servant.API
import           Servant.Client (ClientM, client)

import           Data.Game      (FilteredGameState, GameResult, PlayerAction,
                                 PlayerUUID)
import           Server.API     (gameAPI)

clientHealth :: ClientM T.Text
clientJoin :: ClientM GameResult
clientState :: PlayerUUID -> ClientM FilteredGameState
clientPlay :: PlayerUUID -> PlayerAction -> ClientM GameResult
( clientHealth :<|>
  clientJoin   :<|>
  clientState  :<|>
  clientPlay ) = client gameAPI

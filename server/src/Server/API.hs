{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Server.API where


import           Control.Monad.Except     (throwError)
import           Control.Monad.IO.Class   (liftIO)
import           Control.Monad.Writer     (tell)
import           Data.Aeson               (encode)
import           Data.IORef               (IORef, newIORef, readIORef,
                                           writeIORef)
import           Data.Proxy               (Proxy (..))
import           Data.Text                as T
import           Network.Wai.Handler.Warp (run)
import           Servant.API
import           Servant.Server


import           Data.Game
import qualified Data.Game                as DG
import           Game                     (getState, join)
import           GameMonad                (GameMonad, runGameMonad')
import           Server.Types             (Valid, invalid)

type GameAPI
  = "health" :> Get '[JSON] T.Text
  :<|> "join" :> Post '[JSON] GameResult
  :<|> "state" :> Capture "uuid" PlayerUUID :> Post '[JSON] FilteredGameState

gameAPI :: Proxy GameAPI
gameAPI = Proxy

-- Handlers

healthHandler :: GameMonad Text
healthHandler = return "OK"

joinHandler :: GameMonad GameResult
joinHandler = join

stateHandler :: PlayerUUID -> GameMonad FilteredGameState
stateHandler uuid = do
  tell [show uuid]
  getState uuid

gameErrorToServantError :: GameError -> ServantErr
gameErrorToServantError gameError@(DG.Unauthorized _)  =
  err401 { errBody = encode (invalid gameError:: Valid GameError ())}
gameErrorToServantError gameError =
  err300 { errBody = encode (invalid gameError :: Valid GameError ()) }

transformGameMonadToHandler :: IORef GameState -> GameMonad a -> Handler a
transformGameMonadToHandler ref action = do
  gameState <- liftIO $ readIORef ref
  (result, logs) <- liftIO $ runGameMonad' action gameState
  -- TODO: Use a proper logger Monad to display log
  liftIO $ print logs
  case result of
    Left gameError -> do
      -- TODO: Use a proper logger Monad to display log
      liftIO (print gameError)
      throwError $ gameErrorToServantError gameError

    Right (x, s') -> do
      liftIO $ writeIORef ref s'
      return x

  -- TODO: add error handling
  -- where
  --   handler :: SomeException -> IO (Either ServantErr a)
  --   handler e = return $ Left $ err500 { errBody = encode (show e) }

gameServer :: ServerT GameAPI GameMonad
gameServer =
  healthHandler :<|>
    joinHandler :<|>
      stateHandler

runServer :: IO ()
runServer = do

  -- TODO: use STM instead of IORef for game states
  -- to avoid concurrency issues or rely on redis
  -- that handles transactions
  ref <- newIORef emptyGameState

  -- TODO: use config for port and Debug Mode for instance
  run 8092
    $ serve gameAPI
    $ hoistServer gameAPI (transformGameMonadToHandler ref) gameServer

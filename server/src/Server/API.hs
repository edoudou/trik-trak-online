{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Server.API where


import           Control.Monad.IO.Class   (liftIO)
import           Data.Proxy               (Proxy (..))
import           Data.Text                as T
import           Data.UUID                (UUID)
import           Network.Wai.Handler.Warp (run)
import           Servant.API
import           Servant.Server

import           Data.IORef               (IORef, newIORef, readIORef)

import           Data.Game
import           Game                     (join)
import           GameMonad                (AppMonad, runAppMonad)
import           Server.Types
import           Stub

type GameAPI
  = "health" :> Get '[JSON] T.Text
  :<|> "join" :> Post '[JSON] (Valid GameError (UUID, PlayerId))
  :<|> "state" :> Capture "uuid" T.Text :> Get '[JSON] FilteredGameState

gameAPI :: Proxy GameAPI
gameAPI = Proxy

-- Handlers

healthHandler :: AppMonad Text
healthHandler = return "OK"

joinHandler :: AppMonad (Valid GameError (UUID, PlayerId))
joinHandler = eitherToValid <$> join

stateHandler :: T.Text -> AppMonad FilteredGameState
stateHandler _ = return filteredGameState

transformAppMonadToHandler :: IORef GameState -> AppMonad a -> Handler a
transformAppMonadToHandler ref action = do
  gameState <- liftIO $ readIORef ref
  liftIO $ print gameState
  result <- liftIO $ runAppMonad action ref
  case result of
    Left e -> do
      liftIO (print e)
      liftIO (putStrLn "ERROR")
      -- TODO: create Proper payloads for errors
      undefined

    Right x -> do
      liftIO (putStrLn "SUCCESS")
      return x


  -- TODO: add error handling
  -- result <- liftIO $ handler `handle` (runAppAction sqliteInfo redisInfo action)
  -- where
  --   handler :: SomeException -> IO (Either ServantErr a)
  --   handler e = return $ Left $ err500 { errBody = pack (show e) }

gameServer :: ServerT GameAPI AppMonad
gameServer =
  healthHandler :<|>
    joinHandler :<|>
      stateHandler

runServer :: IO ()
runServer = do

  -- IORef for game state
  ref <- newIORef emptyGameState

  -- TODO: use config for port and Debug Mode for instance
  run 8092
    $ serve gameAPI
    $ hoistServer gameAPI (transformAppMonadToHandler ref) gameServer

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Server.API
  ( GameAPI
  , gameAPI
  , gameServer

  , withServer
  , runServer
  , killServer

  , Config
  , cEnvironnment
  , mkConfig

  , Handle
  , hConfig
  , hDB
  , hServerThread
  )

  where


import           Control.Concurrent       (ThreadId, forkIO, killThread, threadDelay)
import           Control.Exception        (bracket)
import           Control.Monad.Except     (throwError)
import           Control.Monad.IO.Class   (liftIO)
import           Control.Monad.State      (get)
import           Control.Monad.Writer     (tell)
import           Data.Aeson               (encode)
import           Data.Proxy               (Proxy (..))
import           Data.Text                as T
import           GHC.Conc                 (TVar, atomically, newTVarIO,
                                           readTVar, writeTVar)
import           Network.Wai.Handler.Warp (run)
import           Servant.API
import           Servant.Server
import           System.Random            (StdGen, getStdGen)


import           Data.Game
import qualified Data.Game                as DG
import           Game                     (checkPlayerTurn, getState, handle,
                                           join, printGameState)
import           GameMonad                (GameMonad, runGameMonad)
import           Server.Environment       (Environment)
import           Server.Types             (Valid, invalid, port)



type GameAPI
  =    "health"                           -- ^ API Health
       :> Get '[JSON] T.Text
  :<|> "join"                             -- ^ Joining a game
       :> Post '[JSON] GameResult
  :<|> "state"                            -- ^ Getting game state for player
       :> Capture "uuid" PlayerUUID
       :> Post '[JSON] FilteredGameState
  :<|> "play"                             -- ^ Playing a Move
       :> Capture "uuid" PlayerUUID
       :> ReqBody '[JSON] PlayerAction
       :> Post '[JSON] GameResult

gameAPI :: Proxy GameAPI
gameAPI = Proxy

-- Handlers

healthHandler :: GameMonad Text
healthHandler = return "OK"

joinHandler :: GameMonad GameResult
joinHandler = join

stateHandler :: PlayerUUID -> GameMonad FilteredGameState
stateHandler uuid = tell [show uuid] >> getState uuid

playHandler :: PlayerUUID -> PlayerAction -> GameMonad GameResult
playHandler uuid playerAction = do
  gameState <- get
  case findPlayer gameState uuid of
    Nothing ->
      throwError $ PlayerNotFound uuid
    Just player ->
      withPlayerTurn player $ handle $ PA player playerAction

withPlayerTurn :: Player -> GameMonad a -> GameMonad a
withPlayerTurn player action = do
  b <- checkPlayerTurn player
  if b
  then action
  else throwError $ WrongPlayerTurn (_uuid player)

gameErrorToServantError :: GameError -> ServantErr
gameErrorToServantError gameError@(DG.Unauthorized _)  =
  err401 { errBody = encode (invalid gameError:: Valid GameError ())}
gameErrorToServantError gameError =
  err300 { errBody = encode (invalid gameError :: Valid GameError ()) }

-- Natural Transformation: GameMonad ~> Handler
transformGameMonadToHandler
  :: GameEnvironment
  -> TVar GameState
  -> GameMonad a
  -> Handler a
transformGameMonadToHandler gameEnv tvar action = do

  (result, logs) <- liftIO $ atomically $ do
    gameState <- readTVar tvar
    let (result', logs') = runGameMonad gameEnv gameState action

    case result' of
      Left _ ->
        return (result', logs')

      Right (_, s') -> do
        writeTVar tvar s'
        return (result', logs')

  -- TODO: use proper monad logger for printing logs
  liftIO $ print logs

  case result of
    Left gameError -> do
      -- TODO: Use a proper logger Monad to display Error
      liftIO $ print gameError
      throwError $ gameErrorToServantError gameError

    Right (x, s) -> do
      liftIO $ printGameState s
      return x

gameServer :: ServerT GameAPI GameMonad
gameServer =
  healthHandler :<|>
  joinHandler   :<|>
  stateHandler  :<|>
  playHandler

-- TODO: add config for Logger there
data Config
  = Config
    { cStdGen       :: StdGen            -- ^ Initial StdGen
    , cGameEnv      :: GameEnvironment   -- ^ Game Environment
    , cEnvironnment :: Environment       -- ^ Environment to run the server
    }

-- TODO: add a Logger Resource in there
data Handle
  = Handle
    { hDB           :: TVar GameState  -- ^ In memory Database
    , hConfig       :: Config          -- ^ Server Config
    , hServerThread :: ThreadId        -- ^ Server threadId
    }

withServer :: Config -> (Handle -> IO a) -> IO a
withServer config = bracket (startServer config) killServer

mkConfig :: Environment -> IO Config
mkConfig env = do
  gen <- liftIO getStdGen
  return $ Config gen defaultGameEnvironment env

startServer :: Config -> IO Handle
startServer config = do
  tvar <- newTVarIO $ emptyGameState (cStdGen config)
  tid <- forkIO
    $ run (port (cEnvironnment config))
    $ serve gameAPI
    $ hoistServer gameAPI
      (transformGameMonadToHandler (cGameEnv config) tvar)
      gameServer

  -- Hack :( wait till the server is properly started
  threadDelay 1000

  return $ Handle tvar config tid

killServer :: Handle -> IO ()
killServer = killThread . hServerThread

runServer :: Environment -> IO Handle
runServer env = do
  config <- mkConfig env
  startServer config

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GameMonad
  ( GameMonad
  , runGameMonad
  , evalGameMonadLog
  , evalGameMonad
  , execGameMonad
  , execGameMonadState
  )
  where

import           Control.Monad.Error.Class  (MonadError)
import           Control.Monad.Random       (MonadRandom, Rand, StdGen, runRand)
import           Control.Monad.Reader.Class (MonadReader)
import           Control.Monad.State.Class  (MonadState)
import           Control.Monad.Trans.Except (ExceptT, runExceptT)
import           Control.Monad.Trans.Reader (ReaderT, runReaderT)
import           Control.Monad.Trans.State  (StateT, runStateT)
import           Control.Monad.Trans.Writer (WriterT, runWriterT)
import           Control.Monad.Writer.Class (MonadWriter)

import           Data.Game                  (GameEnvironment, GameError,
                                             GameLog, GameState, _gstGen)


newtype GameMonad a
  = GameMonad (ReaderT GameEnvironment
                       (StateT GameState
                               (ExceptT GameError
                                        (WriterT GameLog
                                                 (Rand StdGen)))) a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadError GameError
    , MonadState GameState
    , MonadWriter GameLog
    , MonadReader GameEnvironment
    , MonadRandom
    )

runGameMonad
  :: GameEnvironment
  -> GameState
  -> GameMonad a
  -> (Either GameError (a, GameState), GameLog)
runGameMonad gameEnv gameState (GameMonad action) =
  (result', logs)

  where
    randomAction = runWriterT
      $ runExceptT
      $ flip runStateT gameState
      $ runReaderT action gameEnv

    ((result, logs), gen') = runRand randomAction (_gstGen gameState)

    -- TODO: use lens to simplify this boilerplate
    result' = (\(x, s) -> (x, s { _gstGen = gen' })) <$> result

evalGameMonadLog
  :: GameEnvironment
  -> GameState
  -> GameMonad a
  -> GameLog
evalGameMonadLog gameEnv gameState action =
  snd $ runGameMonad gameEnv gameState action

evalGameMonad
  :: GameEnvironment
  -> GameState
  -> GameMonad a
  -> Either GameError (a, GameState)
evalGameMonad gameEnv gameState action =
  fst $ runGameMonad gameEnv gameState action

execGameMonadState
  :: GameEnvironment
  -> GameState
  -> GameMonad a
  -> Either GameError GameState
execGameMonadState gameEnv gameState action =
  snd <$> evalGameMonad gameEnv gameState action

execGameMonad
  :: GameEnvironment
  -> GameState
  -> GameMonad a
  -> Either GameError a
execGameMonad gameEnv gameState action =
  fst <$> evalGameMonad gameEnv gameState action

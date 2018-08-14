{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GameMonad where

import           Control.Monad.Error.Class  (MonadError)
import           Control.Monad.Random       (MonadRandom, Rand, StdGen,
                                             evalRand)
import           Control.Monad.Reader.Class (MonadReader)
import           Control.Monad.State.Class  (MonadState)
import           Control.Monad.Trans.Except (ExceptT, runExceptT)
import           Control.Monad.Trans.Reader (ReaderT, runReaderT)
import           Control.Monad.Trans.State  (StateT, runStateT)
import           Control.Monad.Trans.Writer (WriterT, runWriterT)
import           Control.Monad.Writer.Class (MonadWriter)

import           Data.Game


newtype GameMonad a
  = GameMonad (ReaderT GameEnvironment
                       (StateT GameState
                               (ExceptT GameError
                                        (WriterT GameLog
                                                 (Rand StdGen)))) a)
  deriving
    ( MonadError GameError
    , MonadState GameState
    , MonadWriter GameLog
    , MonadReader GameEnvironment
    , MonadRandom
    , Functor
    , Applicative
    , Monad
    )

-- TODO: Refactor
runGameMonad :: GameEnvironment -> GameState -> GameMonad a -> (Either GameError (a, GameState), GameLog)
runGameMonad gameEnv gameState (GameMonad action) =
  flip evalRand (gen gameEnv)
    $ runWriterT
    $ runExceptT
    $ flip runStateT gameState
    $ runReaderT action gameEnv

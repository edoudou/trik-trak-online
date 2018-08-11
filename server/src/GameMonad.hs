{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GameMonad where

import           Control.Monad.Error.Class  (MonadError)
import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.State.Class  (MonadState)
import           Control.Monad.Trans.Except (ExceptT, runExceptT)
import           Control.Monad.Trans.State  (StateT, runStateT)
import           Control.Monad.Trans.Writer (WriterT, runWriterT)
import           Control.Monad.Writer.Class (MonadWriter)

import           Data.Game


newtype GameMonad a
  = GameMonad (StateT GameState (ExceptT GameError (WriterT [String] IO)) a)
  deriving
    ( MonadError GameError
    , MonadIO
    , MonadState GameState
    , MonadWriter [String]
    , Functor
    , Applicative
    , Monad
    )

runGameMonad' :: GameMonad a -> GameState -> IO (Either GameError (a, GameState), [String])
runGameMonad' (GameMonad action) =
  runWriterT . runExceptT . runStateT action

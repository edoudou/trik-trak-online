{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GameMonad where

import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Trans.Except (ExceptT, runExceptT)
import           Control.Monad.Trans.State  (StateT, runStateT)
import           Data.IORef                 (IORef, readIORef, writeIORef)

import           Data.GameState
import           Types


newtype AppMonad a = AppMonad (StateT GameState (ExceptT GameError IO) a)
  deriving (Functor, Applicative, Monad, MonadIO)

runAppMonad :: AppMonad a -> IORef GameState -> IO (Either GameError a)
runAppMonad (AppMonad action) ref = do
    gameState <- liftIO $ readIORef ref
    result    <- runExceptT $ runStateT action gameState
    case result of
      Left e -> return (Left e)
      Right (val, s') -> do
        writeIORef ref s'
        return $ Right val

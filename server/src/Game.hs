{-# LANGUAGE ScopedTypeVariables #-}

module Game where

import           Control.Monad.Except   (throwError)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.State    (get, modify')
import           Control.Monad.Writer   (tell)
import qualified Data.Set               as S


import           Data.Game
import           GameMonad

isOver :: GameState -> Bool
isOver _ = undefined

play :: Action -> GameMonad ()
play _  = do
    _ <- get
    return ()

handle :: Action -> GameMonad GameResult
handle (NPA JoinGame) = join
handle _              = undefined

checkPlayerNumber :: GameMonad ()
checkPlayerNumber = do
  gameState <- get
  let nPlayers = length (_players gameState)
  if nPlayers /= 4
  then throwError $ WrongNumberPlayers nPlayers
  else return ()

mkDeck :: GameMonad Deck
mkDeck = do
  deck <- liftIO $ shuffleDeck defaultDeck
  modify' $ addDeck deck
  return deck

  where
    addDeck :: Deck -> GameState -> GameState
    addDeck deck s = s { _deck = deck }

initGame :: GameMonad GameResult
initGame = do
  checkPlayerNumber
  mkDeck
  return Unit

join :: GameMonad GameResult
join = do
  gameState <- get
  case nextPlayerId (_players gameState) of
    Nothing       -> do
      tell ["Cannot add a new player, the party is full"]
      throwError JoinTooManyPlayers

    Just playerId -> do
      player <- liftIO $ mkPlayer playerId emptyHand defaultPegs
      modify' $ addPlayer player
      tell ["Adding new Player to the Game" ++ show player]
      return $ NewPlayer (_uuid player) (_id player)

  where
    addPlayer :: Player -> GameState -> GameState
    addPlayer player s = s { _players = S.insert player (_players s) }

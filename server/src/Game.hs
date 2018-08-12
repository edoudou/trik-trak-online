{-# LANGUAGE ScopedTypeVariables #-}

module Game where

import           Control.Monad          (when)
import           Control.Monad.Except   (throwError)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.State    (get, modify')
import           Control.Monad.Writer   (tell)
import qualified Data.Set               as S


import           Data.Game
import           GameMonad


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

initGame :: GameMonad ()
initGame = do
  checkPlayerNumber
  mkDeck
  return ()

deal :: GameMonad ()
deal = do
  gameState <- get
  case dealCards (_deck gameState) of
    Nothing -> do
      tell ["Not enough cards in Deck, making a new Deck"]
      mkDeck
      deal
    Just (deck', hands) -> do
      tell ["Dealing cards from deck"]
      modify' (\s -> s { _deck = deck'
                       , _players = giveHand hands (_players gameState)
                       -- TODO: add something into game state to cycle between players
                       , _mode = Play P1
                       })
  where
    giveHand :: [Hand] -> S.Set Player -> S.Set Player
    giveHand hands players =
      S.fromList [p { _cards = h } | (h, p) <- (zip hands (S.elems players))]


join :: GameMonad GameResult
join = do
  gameState <- get
  case nextPlayerId (_players gameState) of
    Nothing       -> do
      tell ["Cannot add a new player, the party is full"]
      throwError JoinTooManyPlayers

    Just playerId -> do
      player <- liftIO $ mkPlayer playerId emptyHand defaultPegs
      tell ["Adding new Player to the Game" ++ show player]
      modify' $ addPlayer player
      when (length (_players gameState) == 3) deal
      return $ NewPlayer (_uuid player) (_id player)

  where
    addPlayer :: Player -> GameState -> GameState
    addPlayer player s = s { _players = S.insert player (_players s) }

getState :: PlayerUUID -> GameMonad FilteredGameState
getState uuid = do
  gameState <- get
  tell ["[uuid: " ++ show uuid ++ "] Requesting GameState"]
  case findPlayer gameState uuid of
    Nothing     -> throwError $ Unauthorized uuid
    Just player -> return $ filterGameState player gameState

{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GameSpec where

import           Control.Monad         (forM, forM_, replicateM, void)
import           Control.Monad.State   (get)
import qualified Data.Set              as S
import           System.Random         (getStdGen)

import           Data.UUID.V4          (nextRandom)
import           System.Random.Shuffle (shuffleM)
import           Test.Tasty.Hspec      (Spec, before, beforeAll, describe, it,
                                        shouldBe, shouldContain,
                                        shouldMatchList, shouldNotContain)

import           Data.Game
import           Game                  (exchangeCard, getState, join, mkDeck,
                                        quitGame)
import           GameMonad             (GameMonad, evalGameMonad, execGameMonad)
import           SpecUtils             (allPlayerIds)

spec :: Spec
spec =
  describe "Game" $ do

    before setupGameEnvState $
      describe "Game.mkDeck" $
        it "contains 48 cards" $ \(gameEnv, gameState) ->
          case evalGameMonad gameEnv gameState mkDeck of
            Right (deck, _) -> length deck `shouldBe` 48
            Left e          -> fail $ "Error: " ++ show e

    before setupGameEnvState $
      describe "Game.quitGame" $ do
        let quitAction :: GameMonad Player
            quitAction = do
              joinPlayers
              exchangeRandomCards
              quitRandomPlayerGame

        it "Game Mode is `Winner`" $ \(gameEnv, gameState) ->
          case evalGameMonad gameEnv gameState quitAction of
            Right (_, GameState {..}) ->
              case _mode of
                Winner _ -> True `shouldBe` True
                _        -> fail "Error: wrong game mode"
            Left _ -> fail "Error"

        it "the player leaving the game is not among the winners" $ \(gameEnv, gameState) ->
          case evalGameMonad gameEnv gameState quitAction of
            Right (Player {..}, GameState {..}) ->
              case _mode of
                Winner (Team pid1 pid2) ->
                  [pid1, pid2] `shouldNotContain` [_id]
                _ -> fail "Error: wrong game mode"
            Left _ -> fail "Error"

    describe "Game.join" $ do

      before (joinNPlayers 1) $
        describe "1 attempt" $
          it "allows 1 player to join" $ \case
            Right [(_, pid)] -> pid `shouldBe` P1
            _                       -> fail "Error"

      before (joinNPlayers 4) $
        describe "4 attempts" $
          it "allows 4 players to join" $ \case
            Right xs -> snd <$> xs `shouldContain` allPlayerIds
            _        -> fail "Error"

      before (joinNPlayers 5) $
        describe "5 attempts" $
          it "Throws a GameError" $ \case
            Left e -> e `shouldBe` JoinTooManyPlayers
            _      -> fail "Error"

    describe "Game.getState" $ do

      before setupGameEnvState $
        describe "0 player" $
          it "is Unauthorized" $
            \(gameEnv, gameState) -> do
              uuid <- nextRandom
              case execGameMonad gameEnv gameState (getState uuid) of
                Left e  -> e `shouldBe` Unauthorized uuid
                Right _ -> fail "Error"

      beforeAll joinAndGetState $
        describe "1 player" $ do

          it "returns FilteredGameState" $ \case
            Right FilteredGameState {} -> True `shouldBe` True
            _                            -> fail "Error"

          it "Game Mode is JoinWait" $ \case
            Right FilteredGameState {..} -> _fmode `shouldBe` JoinWait
            _                            -> fail "Error"

          it "has cards for P1" $ \case
            Right FilteredGameState {..} -> fst <$> _fcards `shouldContain` [P1]
            _                            -> fail "Error"

          it "has pegs for P1" $ \case
            Right FilteredGameState {..} -> fst <$> _fpegs `shouldContain` [P1]
            _                            -> fail "Error"

      beforeAll (joinNPlayersAndGetStates 4) $
        describe "4 players" $ do

          it "returns 4 PlayerIds" $ \case
            Right xs -> (fst <$> xs) `shouldMatchList` allPlayerIds
            _        -> fail "Error"

          it "Game Mode is CardExchange" $ \case
            Right xs -> forM_ (snd <$> xs) $ \FilteredGameState {..} ->
              _fmode `shouldBe` CardExchange
            _ -> fail "Error"

          it "players have cards" $ \case
            Right xs -> forM_ (snd <$> xs) $ \FilteredGameState {..} ->
              fst <$> _fcards `shouldMatchList` allPlayerIds
            _ -> fail "Error"

          it "players have pegs" $ \case
            Right xs -> forM_ (snd <$> xs) $ \FilteredGameState {..} ->
              fst <$> _fpegs `shouldMatchList` allPlayerIds
            _ -> fail "Error"


-- TODO: refactor to only run in the GameMonad
joinAndGetState :: IO (Either GameError FilteredGameState)
joinAndGetState = do
  (gameEnv, gameState) <- setupGameEnvState
  return $ execGameMonad gameEnv gameState gameAction
  where
    gameAction :: GameMonad FilteredGameState
    gameAction = do
      (uuid, _) <- join
      getState uuid

-- TODO: refactor to only run in the GameMonad
joinNPlayersAndGetStates :: Int -> IO (Either GameError [(PlayerId, FilteredGameState)])
joinNPlayersAndGetStates n = do
  (gameEnv, gameState) <- setupGameEnvState
  return $ execGameMonad gameEnv gameState gameAction
  where
    gameAction :: GameMonad [(PlayerId, FilteredGameState)]
    gameAction = do
      players <- replicateM n join
      forM players $ \(uuid, pid) -> do
        filteredState <- getState uuid
        return (pid, filteredState)

-- Utils

setupGameEnvState :: IO (GameEnvironment, GameState)
setupGameEnvState = do
  g <- getStdGen
  return (defaultGameEnvironment, emptyGameState g)

-- TODO: refactor to only run in the GameMonad
joinNPlayers :: Int -> IO (Either GameError [(PlayerUUID, PlayerId)])
joinNPlayers n = do
  (gameEnv, gameState) <- setupGameEnvState
  return $ execGameMonad gameEnv gameState $ replicateM n join

-- TODO
exchangeCardForPlayerId :: PlayerId -> GameMonad ()
exchangeCardForPlayerId pid = do
  gameState <- get
  case findPlayerByPlayerId gameState pid of
    Nothing     -> return ()
    Just player -> return ()

joinPlayers :: GameMonad ()
joinPlayers = void $ replicateM 4 join

exchangeRandomCards :: GameMonad ()
exchangeRandomCards = do
  gameState <- get
  forM_ (_players gameState) $ \player -> do
    card <- head <$> shuffleM (_cards player)
    exchangeCard player card
  return ()

quitRandomPlayerGame :: GameMonad Player
quitRandomPlayerGame = do
  gameState <- get
  player <- head <$> shuffleM (S.elems (_players gameState))
  quitGame player
  return player

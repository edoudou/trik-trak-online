{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GameSpec where

import           Control.Monad    (forM, forM_, replicateM)
import qualified Data.Map         as M
import           Data.UUID.V4     (nextRandom)
import           System.Random    (getStdGen)
import           Test.Tasty.Hspec (Spec, before, beforeAll, describe, it,
                                   shouldBe, shouldContain, shouldMatchList)

import           Data.Game        (FilteredGameState (..), GameEnvironment,
                                   GameError (..), GameResult (..), GameState,
                                   Mode (..), PlayerId (..),
                                   defaultGameEnvironment, emptyGameState)
import           Game             (getState, join, mkDeck)
import           GameMonad        (GameMonad, runGameMonad)
import           SpecUtils        (assertAllPlayerIds)

spec :: Spec
spec =
  describe "Game" $ do

    before setupGameEnvState $
      describe "Game.mkDeck" $
        it "contains 48 cards" $ \(gameEnv, gameState) ->
          let (result, _) = runGameMonad gameEnv gameState mkDeck
          in case result of
               Right (deck, _) -> length deck `shouldBe` 48
               Left e          -> fail $ "Error: " ++ show e

    describe "Game.join" $ do

      before (joinNPlayers 1) $
        describe "1 attempt" $
          it "allows 1 player to join" $ \case
            Right [NewPlayer _ pid] -> pid `shouldBe` P1
            _                       -> fail "Error"

      before (joinNPlayers 4) $
        describe "4 attempts" $
          it "allows 4 players to join" $ \case
            Right xs -> assertAllPlayerIds xs
            _        -> fail "Error"

      before (joinNPlayers 5) $
        describe "5 attempts" $
          it "Throws a GameError" $ \case
            Left e -> e `shouldBe` JoinTooManyPlayers
            _      -> fail "Error"

    describe "Game.getState" $ do

      describe "0 player" $
        it "is Unauthorized" $ do
          (gameEnv, gameState) <- setupGameEnvState
          uuid <- nextRandom
          case withGameResult gameEnv gameState (getState uuid) of
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
            Right FilteredGameState {..} -> M.keys _fcards `shouldContain` [P1]
            _                            -> fail "Error"

          it "has pegs for P1" $ \case
            Right FilteredGameState {..} -> M.keys _fpegs `shouldContain` [P1]
            _                            -> fail "Error"

      beforeAll (joinNPlayersAndGetStates 4) $
        describe "4 players" $ do

          it "returns 4 PlayerIds" $ \case
            Right xs -> (fst <$> xs) `shouldMatchList` [P1, P2, P3, P4]
            _        -> fail "Error"

          it "Game Mode is CardExchange" $ \case
            Right xs -> forM_ (snd <$> xs) $ \FilteredGameState {..} ->
              _fmode `shouldBe` CardExchange
            _ -> fail "Error"

          it "players have cards" $ \case
            Right xs -> forM_ (snd <$> xs) $ \FilteredGameState {..} ->
              M.keys _fcards `shouldMatchList`  [P1, P2, P3, P4]
            _ -> fail "Error"

          it "players have pegs" $ \case
            Right xs -> forM_ (snd <$> xs) $ \FilteredGameState {..} ->
              M.keys _fpegs `shouldMatchList`  [P1, P2, P3, P4]
            _ -> fail "Error"


joinAndGetState :: IO (Either GameError FilteredGameState)
joinAndGetState = do
  (gameEnv, gameState) <- setupGameEnvState
  return $ withGameResult gameEnv gameState gameAction
  where
    gameAction :: GameMonad FilteredGameState
    gameAction = do
      NewPlayer uuid pid <- join
      getState uuid

joinNPlayersAndGetStates :: Int -> IO (Either GameError [(PlayerId, FilteredGameState)])
joinNPlayersAndGetStates n = do
  (gameEnv, gameState) <- setupGameEnvState
  return $ withGameResult gameEnv gameState gameAction
  where
    gameAction :: GameMonad [(PlayerId, FilteredGameState)]
    gameAction = do
      players <- replicateM n join
      forM players $ \(NewPlayer uuid pid) -> do
        filteredState <- getState uuid
        return (pid, filteredState)

-- Utils

setupGameEnvState :: IO (GameEnvironment, GameState)
setupGameEnvState = do
  g <- getStdGen
  return (defaultGameEnvironment, emptyGameState g)

withGameResult :: GameEnvironment -> GameState -> GameMonad a -> Either GameError a
withGameResult gameEnv gameState action = fst <$> fst (runGameMonad gameEnv gameState action)

withGameState :: GameEnvironment -> GameState -> GameMonad a -> Either GameError GameState
withGameState gameEnv gameState action = snd <$> fst (runGameMonad gameEnv gameState action)

joinNPlayers :: Int -> IO (Either GameError [GameResult])
joinNPlayers n = do
  (gameEnv, gameState) <- setupGameEnvState
  return $ withGameResult gameEnv gameState $ replicateM n join

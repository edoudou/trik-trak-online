{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server.APISpec
  (spec)
  where

import           Control.Monad             (replicateM, void)
import           Data.Either               (isLeft, isRight, rights)
import           Data.List                 as L
import           Data.Set                  as S
import           GHC.Conc                  (TVar, atomically, readTVarIO,
                                            writeTVar)
import           Network.HTTP.Types.Status (Status (..))
import           Servant.Client            (ClientEnv, GenResponse (Response),
                                            ServantError (FailureResponse),
                                            responseStatusCode, runClientM)
import           Test.Tasty.Hspec          (Expectation, Spec, afterAll_,
                                            after_, describe, it, shouldBe)

import           Data.Game                 (FilteredGameState (..),
                                            GameResult (..), GameState,
                                            Mode (..), PlayerAction,
                                            PlayerId (..), PlayerUUID,
                                            emptyGameState, _fmode, _players)
import qualified Server.API
import           Server.Client             (clientHealth, clientJoin,
                                            clientState)


joinSpec :: Server.API.Handle -> ClientEnv -> Spec
joinSpec handle clientEnv = describe "/join" $ do
  afterAll_ (resetDB handle) $
    describe "1 attempt" $ do
      it "has no players" $ assertNPlayers db 0
      it "returns P1 as the PlayerId" $ do
        result <- runClientM clientJoin clientEnv
        case result of
          Left e                  -> fail $ show e
          Right (NewPlayer _ pid) -> pid `shouldBe` P1
          Right x                 -> fail $ show x

  afterAll_ (resetDB handle) $
    describe "4 attempts" $ do
      it "has no players" $ assertNPlayers db 0
      it "contains all PlayerIds" $ do
        xs <- replicateM 4 $ runClientM clientJoin clientEnv
        assertAllPlayerIds $ rights xs
      it "contains 4 players" $ assertNPlayers db 4

  afterAll_ (resetDB handle) $
    describe "5 attempts" $ do
      it "has no players" $ assertNPlayers db 0
      it "allows only 4 players to join" $ do
        xs <- replicateM 5 $ runClientM clientJoin clientEnv
        all isRight xs `shouldBe` False
        assertAllPlayerIds $ rights xs
      it "contains 4 players" $ assertNPlayers db 4

  afterAll_ (resetDB handle) $
    describe "Too many join requests" $ do
      it "throws a 300 error" $ do
        xs <- replicateM 5 $ runClientM clientJoin clientEnv
        case L.find isLeft xs of
          Nothing       -> fail "No error thrown"
          Just (Left e) -> assertFailureResponseError e 300
      it "contains 4 players" $ assertNPlayers db 4

  where
    db = Server.API.hDB handle

healthSpec :: ClientEnv -> Spec
healthSpec clientEnv = describe "/health" $
  it "is healthy" $ do
  result <- runClientM clientHealth clientEnv
  result `shouldBe` Right "OK"

stateSpec :: Server.API.Handle -> ClientEnv -> Spec
stateSpec handle clientEnv = describe "/state" $ do

  after_ (resetDB handle) $
    describe "1 player" $
      it "has JoinWait as the GameMode" $ do
        r1 <- runClientM clientJoin clientEnv
        case r1 of
          Right (NewPlayer uuid _) -> do
            r2 <- runClientM (clientState uuid) clientEnv
            case r2 of
              Left e                       -> fail (show e)
              Right FilteredGameState {..} -> _fmode `shouldBe` JoinWait
          _ -> fail "fail"

  afterAll_ (resetDB handle) $
    describe "4 players" $ do
      it "has CardExchange as the GameMode" $ do
        xs <- replicateM 4 $ runClientM clientJoin clientEnv
        case L.head xs of
          Right (NewPlayer uuid _) -> do
            r <- runClientM (clientState uuid) clientEnv
            case r of
              Right FilteredGameState {..} -> _fmode `shouldBe` CardExchange
              _                            -> fail "Error"
          _ -> fail "Error"
      it "contains 4 players" $ assertNPlayers db 4

  where
    db = Server.API.hDB handle

spec :: Server.API.Handle -> ClientEnv -> Spec
spec handle clientEnv = describe "Server.API" $ do
  healthSpec clientEnv
  joinSpec handle clientEnv
  stateSpec handle clientEnv

assertFailureResponseError :: ServantError -> Int -> Expectation
assertFailureResponseError (FailureResponse r@Response{}) errorNumber =
  case responseStatusCode r of
    s@(Status _ _) -> statusCode s `shouldBe` errorNumber
    _              -> fail "Error"
assertFailureResponseError _ _ = fail "Not the right response status code"

assertAllPlayerIds :: [GameResult] -> Expectation
assertAllPlayerIds xs =
  let pids = (\(NewPlayer _ pid) -> pid) <$> xs
  in S.fromList pids `shouldBe` S.fromList [P1, P2, P3, P4]

assertNPlayers :: TVar GameState -> Int -> Expectation
assertNPlayers tvar k = do
  gameState <- readTVarIO tvar
  S.size (_players gameState) `shouldBe` k

resetDB :: Server.API.Handle -> IO ()
resetDB handle = void
  $ atomically
  $ writeTVar (Server.API.hDB handle)
  $ emptyGameState (Server.API.cStdGen (Server.API.hConfig handle))

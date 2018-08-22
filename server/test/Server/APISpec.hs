{-# LANGUAGE OverloadedStrings #-}

module Server.APISpec where

import           Servant.Client          (ClientEnv, runClientM)
import           Test.Tasty.Hspec        (Spec, SpecWith, anyException, before,
                                          describe, it, pending, shouldBe,
                                          shouldThrow)

import           Data.Game               (FilteredGameState, GameResult(..),
                                          PlayerAction, PlayerUUID, PlayerId(..))
import           Server.API
import           Server.Client           (clientHealth, clientJoin)


-- TODO: add some utility to wipe out the DB everytime or restore it.

spec :: ClientEnv -> Spec
spec clientEnv =
  describe "Server.API" $ do
    it "is healthy" $ do
      result <- runClientM clientHealth clientEnv
      result `shouldBe` Right "OK"
    -- Fix Data.Game FromJSON GameResult to make it pass
    it "joins a game" $ do
      result <- runClientM clientJoin clientEnv
      print result
      case result of
        Left e -> fail $ show e
        Right (NewPlayer uuid pid) -> pid `shouldBe` P1
        Right x -> fail $ show x

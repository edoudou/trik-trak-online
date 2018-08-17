module GameSpec where

import           Test.Tasty.Hspec (Spec, before, describe, it, shouldBe)
import           System.Random    (getStdGen)

import           Data.Game        (GameEnvironment, GameState,
                                   defaultGameEnvironment, emptyGameState)
import           Game             (mkDeck)
import           GameMonad        (runGameMonad)

setupGameEnvState :: IO (GameEnvironment, GameState)
setupGameEnvState = do
  g <- getStdGen
  return (defaultGameEnvironment, emptyGameState g)

spec :: Spec
spec =
  before setupGameEnvState $
    describe "Game" $
      describe "Game.mkDeck" $
        it "contains 48 cards" $ \(gameEnv, gameState) ->
          let (result, _) = runGameMonad gameEnv gameState mkDeck
          in case result of
               Right (deck, _) -> length deck `shouldBe` 48
               Left e          -> fail $ "Error: " ++ show e

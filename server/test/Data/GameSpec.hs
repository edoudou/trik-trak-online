module Data.GameSpec where

import           Control.Monad    (forM_)
import           Data.List        as L
import           Data.Set         as S
import           System.Random    (getStdGen)
import           Test.Tasty.Hspec (Spec, SpecWith, anyException, before,
                                   describe, it, shouldBe, shouldThrow)

import           Data.Game


spec :: Spec
spec =
  describe "Data.Game" $ do
    describe "Data.Game.dealCards" $ do

      it "cannot deal an empty deck" $
        dealCards [] `shouldBe` Nothing

      it "cannot deal a deck with too few cards" $
        forM_ [0..15] $ \k ->
          dealCards (L.take k defaultDeck) `shouldBe` Nothing

    before (emptyGameState <$> getStdGen) $
      describe "Data.Game.emptyGameState" $ do

        it "The game is not over" $ \s ->
          isOver s `shouldBe` False

        it "There is no winner" $ \s ->
          winner s `shouldBe` Nothing

        it "No player has won" $ \s ->
          L.or (playerWon <$> S.elems (_players s)) `shouldBe` False

-- spec :: Spec
-- spec =
--   describe "Prelude.head" $ do
--      testSpec

--      it "returns the first element of a list" $
--        head [23 ..] `shouldBe` (23 :: Int)

--      it "returns the first element of an *arbitrary* list" $
--        property $ \x xs -> head (x:xs) == (x :: Int)

--      it "throws an exception if used with an empty list" $
--        evaluate (head []) `shouldThrow` anyException

{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.GameSpec where

import           Control.Monad             (forM_)
import           Data.List                 as L
import           Data.Maybe                (fromMaybe)
import           Data.Set                  as S
import           System.Random             (getStdGen, mkStdGen, random)

import           Data.Aeson                (decode, encode)
import qualified Data.ByteString.Lazy      as BL
import qualified Data.ByteString.Lazy.UTF8 as T
import           Test.QuickCheck           hiding (Discard)
import           Test.Tasty.Hspec          (Spec, SpecWith, anyException,
                                            before, describe, it, shouldBe,
                                            shouldThrow)

import           Data.Game

-- TODO: where to put Orphan Instances?
instance Arbitrary PlayerId where
  arbitrary = elements [P1, P2, P3, P4]

instance Arbitrary Card where
  arbitrary = elements [One, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten,  Switch, Twelve]

instance Arbitrary PlayerUUID where
  arbitrary = do
    stdGen <- mkStdGen <$> arbitrary
    let (uuid, _ ) = random stdGen
    return uuid

instance Arbitrary Team where
  arbitrary = elements [Team P1 P3, Team P2 P4]

instance Arbitrary Mode where
  arbitrary = do
    team <- arbitrary
    pid <- arbitrary
    elements
      [ JoinWait
      , CardExchange
      , Winner team
      , Play pid
      ]

instance Arbitrary GameError where
  arbitrary = do
    uuid     <- arbitrary
    nPlayers <- arbitrary
    card     <- arbitrary
    elements
      [ JoinTooManyPlayers
      , WrongNumberPlayers nPlayers
      , Unauthorized uuid
      , WrongPlayerTurn uuid
      , CardNotAvailabe uuid card
      , PlayerNotFound uuid
      , CardAlreadyExchanged card uuid
      ]

instance Arbitrary PegMode where
  arbitrary = elements [Normal, Stake]

instance Arbitrary BoardPosition where
  arbitrary = do
    x <- choose (0, 16 * 4 - 1)
    return $ fromMaybe startPosition (boardPosition x)

instance Arbitrary TargetPosition where
  arbitrary = do
    x <- choose (0, 4)
    return $ fromMaybe (TargetPosition 0) (targetPosition x)

instance Arbitrary Position where
  arbitrary = do
    bp <- arbitrary
    tp <- arbitrary
    elements [BP bp, TP tp]

instance Arbitrary Peg where
  arbitrary = do
    tp   <- arbitrary
    bp   <- arbitrary
    mode <- arbitrary
    elements
      [ PegBoard bp mode
      , PegTarget tp
      , PegHome
      ]

instance Arbitrary Player where
  arbitrary = do
    uuid        <- arbitrary
    pid         <- arbitrary
    numberCards <- choose (0, 4)
    cards       <- vectorOf numberCards arbitrary
    pegs        <- vectorOf 4 arbitrary
    return $ Player uuid pid cards pegs

instance Arbitrary PlayerAction where
  arbitrary = do
    card     <- arbitrary
    peg1     <- arbitrary
    peg2     <- arbitrary
    position <- arbitrary
    pid      <- arbitrary
    elements
      [ Exchange card
      , Move card peg1 position
      , Discard card
      , SwitchPegs pid peg1 peg2
      , QuitGame
      ]

spec :: Spec
spec =
  describe "Data.Game" $ do
    describe "JSON encoding/decoding" $ do

      describe "Data.Game.GameError" $
        it "encodes GameErrors to a string longer than 5 characters" $
          property $ \(e :: GameError) -> BL.length (encode e) >= 5

      describe "Data.Game.PlayerId" $ do
        it "encodes PlayerId properly" $ property $
          \pid -> encode pid == T.fromString (show $ playerIdToInt pid)
        it "encodes and decodes PlayerId" $
          property $ \(pid :: PlayerId) -> decode (encode pid) == Just pid

      describe "Data.Game.Mode" $ do
        it "encodes JoinWait properly" $
          encode JoinWait `shouldBe` "{\"tag\":\"JoinWait\"}"
        it "encodes CardExchange properly" $
          encode CardExchange `shouldBe` "{\"tag\":\"CardExchange\"}"
        it "encodes Play properly" $
          encode (Play P1) `shouldBe` "{\"tag\":\"Play\",\"pid\":1}"
        it "encodes Winner properly" $
          encode (Winner (Team P1 P2)) `shouldBe` "{\"tag\":\"Winner\",\"team\":[1,2]}"
        it "encodes and decodes game `Mode` properly" $
          property $ \(mode :: Mode) -> decode (encode mode) `shouldBe` Just mode

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

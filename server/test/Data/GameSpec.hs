{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.GameSpec where

import           Control.Monad             (forM, forM_)
import qualified Data.Char                 as C
import qualified Data.List                 as L
import           Data.Maybe                (fromMaybe)
import           Data.Monoid               ((<>))
import qualified Data.Set                  as S
import           System.Random             (getStdGen, mkStdGen, random)

import           Data.Aeson                (FromJSON, ToJSON, decode, encode)
import qualified Data.ByteString.Lazy      as BL
import qualified Data.ByteString.Lazy.UTF8 as T
import           Test.QuickCheck           hiding (Discard)
import           Test.QuickCheck.Instances
import           Test.Tasty.Hspec          (Spec, SpecWith, anyException,
                                            before, describe, it, shouldBe,
                                            shouldThrow)

import           Data.Game

-- TODO: where to put Orphan Instances?
instance Arbitrary PlayerId where
  arbitrary = elements [P1, P2, P3, P4]

instance Arbitrary Card where
  arbitrary = elements [One, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten,  Switch, Twelve]

-- Comes from quickcheck-instances package
-- instance Arbitrary PlayerUUID where
--   arbitrary = do
--     stdGen <- mkStdGen <$> arbitrary
--     let (uuid, _ ) = random stdGen
--     return uuid

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

instance Arbitrary GameResult where
  arbitrary = do
    uuid <- arbitrary
    pid <- arbitrary
    elements
      [ NewPlayer uuid pid
      , Unit
      ]

instance Arbitrary FilteredGameState where
  arbitrary = do
    let teams = (Team P1 P3, Team P2 P4)
    let pids = [P1, P2, P3, P4]
    mode <- arbitrary
    pid <- arbitrary
    nCards <- choose (0, 4)
    cards <- forM pids $ \id -> do
      cards' <- vectorOf nCards arbitrary
      return (id, if pid == id then setVisible <$> cards' else setHidden <$> cards')
    pegs <- forM pids $ \id -> do
      pegs' <- vectorOf nCards arbitrary
      return (id, pegs')
    nActions <- arbitrary
    nHistory <- arbitrary
    actions <- vectorOf nActions arbitrary
    history <- vectorOf nHistory arbitrary
    return $ FilteredGameState mode teams cards pegs actions history

encodeDecodeIdentityProp :: (ToJSON a, FromJSON a, Eq a) => a -> Bool
encodeDecodeIdentityProp x = decode (encode x) == Just x

outputAlphabetProp :: (ToJSON a) => a -> Bool
outputAlphabetProp x =
    used `S.isSubsetOf` allowed
  where
    used = S.fromList . BL.unpack $ encode x
    allowed = S.fromList . map (fromIntegral . C.ord) $ ['A'..'Z'] <> ['a'..'z'] <> ['0'..'9'] <> ['[', ']','{','}',',',':', '_', '"', '\\', '-']

spec :: Spec
spec =
  describe "Data.Game" $ do
    describe "JSON encoding/decoding" $ do
      describe "Data.Game.FilteredGameState" $ do
        it "encodes/decodes FilteredGameState properly" $
          property (encodeDecodeIdentityProp :: FilteredGameState -> Bool)
        it "encodes FilteredGameState with correct alphabet" $
          property (outputAlphabetProp :: FilteredGameState -> Bool)

      describe "Data.Game.Card" $ do
        it "encodes/decodes Card properly" $
          property (encodeDecodeIdentityProp :: Card -> Bool)
        it "encodes Card with correct alphabet" $
          property (outputAlphabetProp :: Card -> Bool)

      describe "Data.Game.Peg" $ do
        it "encodes/decodes Peg properly" $
          property (encodeDecodeIdentityProp :: Peg -> Bool)
        it "encodes Peg with correct alphabet" $
          property (outputAlphabetProp :: Peg -> Bool)

      describe "Data.Game.Position" $ do
        it "encodes/decodes Position properly" $
          property (encodeDecodeIdentityProp :: Position -> Bool)
        it "encodes Position with correct alphabet" $
          property (outputAlphabetProp :: Position -> Bool)

      describe "Data.Game.PegMode" $ do
        it "encodes/decodes PegMode properly" $
          property (encodeDecodeIdentityProp :: PegMode -> Bool)
        it "encodes PegMode with correct alphabet" $
          property (outputAlphabetProp :: PegMode -> Bool)

      describe "Data.Game.PlayerAction" $ do
        it "encodes/decodes PlayerAction" $
          property (encodeDecodeIdentityProp :: PlayerAction -> Bool)
        it "encodes PlayerAction with correct alphabet" $
          property (outputAlphabetProp :: PlayerAction -> Bool)

      describe "Data.Game.GameResult" $ do
        it "encodes/decodes GameResult" $
          property (encodeDecodeIdentityProp :: GameResult -> Bool)
        it "encodes PlayerAction with correct alphabet" $
          property (outputAlphabetProp :: GameResult -> Bool)

      describe "Data.Game.GameError" $ do
        it "encodes GameErrors to a string longer than 5 characters" $
          property $ \(e :: GameError) -> BL.length (encode e) >= 5
        it "encodes GameError with correct alphabet" $
          property (outputAlphabetProp :: GameError -> Bool)

      describe "Data.Game.PlayerId" $ do
        it "encodes PlayerId properly" $ property $
          \pid -> encode pid == T.fromString (show $ playerIdToInt pid)
        it "encodes/decodes PlayerId properly" $
          property (encodeDecodeIdentityProp :: PlayerId -> Bool)
        it "encodes PlayerId with correct alphabet" $
          property (outputAlphabetProp :: PlayerId -> Bool)

      describe "Data.Game.Mode" $ do
        it "encodes/decodes Mode properly" $
          property (encodeDecodeIdentityProp :: Mode -> Bool)
        it "encodes Mode with correct alphabet" $
          property (outputAlphabetProp :: Mode -> Bool)

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

{-# LANGUAGE OverloadedStrings #-}

module Types where

import           Data.Aeson
import           Data.Foldable         (maximum)
import           Data.Function         (on)
import qualified Data.List             as L
import qualified Data.Map              as M
import qualified Data.Set              as S
import qualified Data.Text             as T
import           Data.UUID             (UUID)
import           Data.UUID.V4          (nextRandom)
import           System.Random.Shuffle (shuffleM)

data GameError
  = InvalidAction Player Action
  | JoinTooManyPlayers
  deriving (Eq, Show)

instance ToJSON GameError where
  toJSON (InvalidAction p _) = object
    [ "type"     .= ("InvalidAction" :: T.Text)
    , "player_id" .= _id p
    ]
  toJSON JoinTooManyPlayers = object
    [ "type" .= ("JoinTooManyPlayers" :: T.Text)]

data Action
  = Move Card Position
  deriving (Eq, Show)

data Player = Player
  { _uuid  :: UUID
  , _id    :: PlayerTurn
  , _cards :: Hand
  , _pegs  :: [Peg]
  }
  deriving (Eq, Show)

instance Ord Player where
  compare = compare `on` _id

emptyHand :: Hand
emptyHand = []

defaultPegs :: [Peg]
defaultPegs = L.take 4 $ L.repeat PegHome

mkPlayer :: PlayerTurn -> Hand -> [Peg] -> IO Player
mkPlayer turn hand pegs = do
  uuid <- nextRandom
  return $ Player uuid turn hand pegs

data PegMode
  = Normal
  | Stake
  deriving (Eq, Show)

instance ToJSON PegMode where
  toJSON mode = object
    [ "mode" .= T.pack (show mode)
    ]

newtype Position = Position Int
  deriving (Eq, Show)

data Peg
  = PegBoard Position PegMode
  | PegTarget Position
  | PegHome
  deriving (Eq, Show)

instance ToJSON Peg where
  toJSON PegHome = object
    [ "location" .= (T.pack "Home")
    ]
  toJSON (PegTarget (Position x)) = object
    [ "location" .= (T.pack "Target")
    , "position" .= x
    ]
  toJSON (PegBoard (Position x) mode) = object
    [ "location" .= (T.pack "Board")
    , "position" .= x
    , "stake" .= (mode == Stake :: Bool)
    ]

data Card
  = One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Twelve
  | Switch
  deriving (Eq, Show)

instance ToJSON Card where
  toJSON = toJSON . cardToInt

type Deck       = [Card]
type Hand       = [Card]
type PlayerTurn = Int

cardToInt :: Card -> Int
cardToInt One    = 1
cardToInt Two    = 2
cardToInt Three  = 3
cardToInt Four   = 4
cardToInt Five   = 5
cardToInt Six    = 6
cardToInt Seven  = 7
cardToInt Eight  = 8
cardToInt Nine   = 9
cardToInt Ten    = 10
cardToInt Switch = 11
cardToInt Twelve = 12

defaultDeck :: Deck
defaultDeck =
    L.concat
    [ take 4 (L.repeat c) | c <-
        [ One, Two, Three, Four
        , Five, Six, Seven, Eight
        , Nine, Ten, Switch, Twelve
        ]
    ]

shuffleDeck :: Deck -> IO Deck
shuffleDeck = shuffleM

nextPlayerTurn :: S.Set Player -> PlayerTurn
nextPlayerTurn xs
  | S.null xs = 1
  | otherwise = inc $ maximum $ S.toList $ S.map _id xs
  where
    inc :: PlayerTurn -> PlayerTurn
    inc = (+1)

data GameState = GameState
  { _turn    :: PlayerTurn
  , _over    :: Bool
  , _players :: S.Set Player
  }
  deriving (Eq, Show)

emptyGameState :: GameState
emptyGameState = GameState 0 False S.empty

data Visibility a
  = Visible a
  | Hidden a
  deriving (Eq, Show)

setVisible :: a -> Visibility a
setVisible = Visible

setHidden :: a -> Visibility a
setHidden = Hidden

instance ToJSON a => ToJSON (Visibility a) where
  toJSON (Visible x) = toJSON x
  toJSON (Hidden _)  = toJSON ("hidden" :: T.Text)

data FilteredGameState = FilteredGameState
  { _fturn  :: PlayerTurn
  , _fover  :: Bool
  , _fcards :: M.Map PlayerTurn [Visibility Card]
  , _fpegs  :: M.Map PlayerTurn [Peg]
  }

instance ToJSON FilteredGameState where
  toJSON s = object
    [ "turn" .= _fturn s
    , "over" .= _fover s
    , "pegs" .= _fpegs s
    , "cards" .= _fcards s
    ]

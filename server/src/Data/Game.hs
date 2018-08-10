{-# LANGUAGE OverloadedStrings #-}

module Data.Game where

import           Data.Aeson
import qualified Data.Aeson.Encoding as AE
import           Data.Foldable         (maximum)
import           Data.Function         (on)
import qualified Data.List             as L
import qualified Data.Map              as M
import qualified Data.Set              as S
import qualified Data.Text             as T
import           Data.UUID             (UUID)
import           Data.UUID.V4          (nextRandom)
import           System.Random.Shuffle (shuffleM)

data Mode
  = JoinWait      -- ^ Waiting for other players to join
  | DealCards     -- ^ Dealing cards for all players
  | CardExchange  -- ^ Exchanging cards between teammates
  | Play          -- ^ Regular round play
  | End           -- ^ End of the Game
  deriving (Eq, Show)

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
  = Give Card PlayerId      -- ^ Give Card to PlayerId
  | Move Card Peg Position  -- ^ Move Peg with Card to new Position
  | SwitchPegs Peg Peg      -- ^ Switch two pegs
  | Join                    -- ^ Join the Game
  deriving (Eq, Show)

data Player = Player
  { _uuid  :: UUID      -- ^ Player uuid to issue commands
  , _id    :: PlayerId  -- ^ Player id
  , _cards :: Hand      -- ^ Hand of cards
  , _pegs  :: [Peg]     -- ^ Pegs
  }
  deriving (Eq, Show)

instance Ord Player where
  compare = compare `on` _id

emptyHand :: Hand
emptyHand = []

defaultPegs :: [Peg]
defaultPegs = L.take 4 $ L.repeat PegHome

mkPlayer :: PlayerId -> Hand -> [Peg] -> IO Player
mkPlayer turn hand pegs = do
  uuid <- nextRandom
  return $ Player uuid turn hand pegs

data PegMode
  = Normal  -- ^ Normal Mode
  | Stake   -- ^ Stake Mode
  deriving (Eq, Show)

instance ToJSON PegMode where
  toJSON mode = object
    [ "mode" .= T.pack (show mode)
    ]

newtype Position = Position Int
  deriving (Eq, Show)

data Peg
  = PegBoard Position PegMode  -- ^ On the board at position with mode
  | PegTarget Position         -- ^ On the target at position
  | PegHome                    -- ^ Home
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
data PlayerId
  = P1
  | P2
  | P3
  | P4
  deriving (Eq, Show, Ord)

playerIdToInt :: PlayerId -> Int
playerIdToInt P1 = 1
playerIdToInt P2 = 2
playerIdToInt P3 = 3
playerIdToInt P4 = 4

intToPlayerId :: Int -> Maybe PlayerId
intToPlayerId 1 = Just P1
intToPlayerId 2 = Just P2
intToPlayerId 3 = Just P3
intToPlayerId 4 = Just P4
intToPlayerId _ = Nothing

instance ToJSON PlayerId where
  toJSON = toJSON . playerIdToInt

instance ToJSONKey PlayerId where
  toJSONKey = ToJSONKeyText f g
    where f = T.pack . show
          g = AE.text . T.pack . show
          -- text function is from Data.Aeson.Encoding

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

nextPlayerId :: S.Set Player -> Maybe PlayerId
nextPlayerId xs
  | S.null xs = Just P1
  | otherwise = intToPlayerId
      $ inc
      $ maximum
      $ fmap playerIdToInt
      $ S.toList
      $ S.map _id xs
  where
    inc :: Int -> Int
    inc = (+1)

data GameState = GameState
  { _turn    :: Maybe UUID        -- ^ Uuid of player who should play next
  , _players :: S.Set Player      -- ^ Set of players in the game
  , _deck    :: Deck              -- ^ Current deck of card
  , _mode    :: Mode              -- ^ Mode of the Game
  }
  deriving (Eq, Show)

emptyGameState :: GameState
emptyGameState = GameState
  { _mode    = JoinWait
  , _players = S.empty
  , _deck    = defaultDeck
  , _turn    = Nothing
  }

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
  { _fmode  :: Mode
  , _fturn  :: PlayerId
  , _fcards :: M.Map PlayerId [Visibility Card]
  , _fpegs  :: M.Map PlayerId [Peg]
  }

instance ToJSON FilteredGameState where
  toJSON s = object
    [ "turn"  .= toJSON (_fturn s)
    , "pegs"  .= _fpegs s
    , "cards" .= _fcards s
    ]


-- handle :: UUID -> Action -> GameMonad
-- handle uuid action = undefined

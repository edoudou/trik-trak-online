module Data.GameState where

import Data.Foldable (maximum)
import qualified Data.List             as L
import qualified Data.Map              as M
import qualified Data.Set              as S
import           Data.UUID             (UUID)
import           Data.UUID.V4          (nextRandom)
import           System.Random.Shuffle (shuffleM)

data GameState = GameState
  { _turn    :: PlayerTurn
  , _over    :: Bool
  , _players :: S.Set Player
  }
  deriving (Eq, Show)

emptyGameState :: GameState
emptyGameState = GameState 0 False S.empty

-- TODO: Add Json Instances
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

newtype Position = Position Int
  deriving (Eq, Show)

data Peg
  = PegBoard Position PegMode
  | PegTarget Position
  | PegHome
  deriving (Eq, Show)

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

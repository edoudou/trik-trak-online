{-# LANGUAGE OverloadedStrings #-}

module Data.Game where

import           Data.Aeson
import qualified Data.Aeson.Encoding   as AE
import           Data.Foldable         (maximum)
import           Data.Function         (on)
import qualified Data.List             as L
import qualified Data.Map              as M
import           Data.Maybe            (fromMaybe, maybe)
import qualified Data.Set              as S
import qualified Data.Text             as T
import           Data.UUID             (UUID)
import           Data.UUID.V4          (nextRandom)
import           System.Random.Shuffle (shuffleM)

data Mode
  = JoinWait       -- ^ Waiting for other players to join
  -- | DealCards      -- ^ Dealing cards for all players
  | CardExchange   -- ^ Exchanging cards between teammates
  | Winner Team    -- ^ End of the game with a winner team
  | Play PlayerId  -- ^ Regular round play with PlayerId playing
  deriving (Eq, Show)

instance ToJSON Mode where
  toJSON (Winner team) = object [ "type" .= ("Winner" :: T.Text), "team" .= team ]
  toJSON (Play pid) = object [ "type" .= ("Play" :: T.Text), "pid" .= pid]
  toJSON mode = object [ "type" .= show mode ]

data GameError
  = InvalidAction Action         -- ^ InvalidAction for given Action and Player
  | JoinTooManyPlayers           -- ^ Too many players are already in the Game
  | WrongNumberPlayers Int       -- ^ Wrong Number of players
  | Unauthorized PlayerUUID      -- ^ Unauthorized UUID accessing the game state
  deriving (Eq, Show)

instance ToJSON GameError where
  toJSON (InvalidAction (PA p _)) = object
    [ "type"     .= ("InvalidAction" :: T.Text)
    , "player_id" .= _id p
    ]
  toJSON JoinTooManyPlayers = object
    [ "type" .= ("JoinTooManyPlayers" :: T.Text)]
  toJSON (Unauthorized uuid) = object
    [ "type" .= ("Unauthorized for uuid: " ++ show uuid) ]

data GameResult
  = NewPlayer PlayerUUID PlayerId
  | Unit
  deriving (Eq, Show)

instance ToJSON GameResult where
  toJSON (NewPlayer uuid pid) = object
    [ "uuid" .= uuid
    , "pid"  .= pid
    ]
  toJSON Unit = toJSON ("OK" :: T.Text)


type PlayerUUID = UUID  -- ^ Type synonym for Player UUID

data Action
  = PA Player PlayerAction
  | NPA NoPlayerAction
  deriving (Eq, Show)

data PlayerAction
  = Give Card PlayerId      -- ^ Give Card to PlayerId
  | Move Card Peg Position  -- ^ Move Peg with Card to new Position
  | Discard Card            -- ^ Discard Card
  | SwitchPegs PlayerId Peg Peg   -- ^ Switch two pegs inclluding Player Id
  | QuitGame                -- ^ Quit the game
  deriving (Eq, Show)

data NoPlayerAction
  = JoinGame
  deriving (Eq, Show)

data Player = Player
  { _uuid  :: PlayerUUID -- ^ Player uuid
  , _id    :: PlayerId   -- ^ Player id
  , _cards :: Hand       -- ^ Hand of cards
  , _pegs  :: [Peg]      -- ^ Pegs
  }
  deriving (Eq, Show)

instance Ord Player where
  compare = compare `on` _id

emptyHand :: Hand
emptyHand = []

defaultPegs :: [Peg]
defaultPegs = L.take 4 $ L.repeat PegHome

mkPlayer :: PlayerId -> Hand -> [Peg] -> IO Player
mkPlayer pid hand pegs = do
  uuid <- nextRandom
  return $ Player uuid pid hand pegs

data PegMode
  = Normal  -- ^ Normal Mode
  | Stake   -- ^ Stake Mode
  deriving (Eq, Show)

instance ToJSON PegMode where
  toJSON mode = object
    [ "mode" .= T.pack (show mode)
    ]

newtype Position
  = Position Int  -- ^ Position on the board or the Target
  deriving (Eq, Show)

data Peg
  = PegBoard Position PegMode  -- ^ On the board at position with mode
  | PegTarget Position         -- ^ On the target at position
  | PegHome                    -- ^ Home
  deriving (Eq, Show)

onTarget :: Peg -> Bool
onTarget (PegBoard _ _) = False
onTarget PegHome        = False
onTarget (PegTarget _)  = True

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

data Team = Team PlayerId PlayerId
  deriving (Eq, Show, Ord)

instance ToJSON Team where
  toJSON (Team pid1 pid2) = toJSON (pid1, pid2)

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
    where
      f = T.pack . show . playerIdToInt
      g = AE.text . T.pack . show . playerIdToInt

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
  { _players :: S.Set Player      -- ^ Set of players in the game
  , _deck    :: Deck              -- ^ Current deck of card
  , _mode    :: Mode              -- ^ Mode of the Game
  , _teams   :: (Team, Team)     -- ^ Teams of the game
  }
  deriving (Eq, Show)

emptyGameState :: GameState
emptyGameState = GameState
  { _mode    = JoinWait
  , _players = S.empty
  , _deck    = defaultDeck
  , _teams   = (Team P1 P3, Team P2 P4)
  }

data Visibility a
  = Visible a      -- ^ Visible Element
  | Hidden a       -- ^ Hidden Element
  deriving (Eq, Show, Ord)

setVisible :: a -> Visibility a
setVisible = Visible

setHidden :: a -> Visibility a
setHidden = Hidden

instance ToJSON a => ToJSON (Visibility a) where
  toJSON (Visible x) = toJSON x
  toJSON (Hidden _)  = toJSON ("hidden" :: T.Text)

data FilteredGameState = FilteredGameState
  { _fmode  :: Mode
  , _fteams :: (Team, Team)
  , _fcards :: M.Map PlayerId [Visibility Card]
  , _fpegs  :: M.Map PlayerId [Peg]
  }

instance ToJSON FilteredGameState where
  toJSON s = object
    [ "mode"  .= _fmode s
    , "teams" .= _fteams s
    , "cards" .= _fcards s
    , "pegs"  .= _fpegs s
    ]

findPlayer :: GameState -> PlayerUUID -> Maybe Player
findPlayer s uuid = L.find ((== uuid) . _uuid) players
  where
    players :: [Player]
    players = S.elems (_players s)

getPlayerUUIDs :: GameState -> S.Set PlayerUUID
getPlayerUUIDs = S.map _uuid . _players

playerUUIDToPlayerId :: GameState -> PlayerUUID -> Maybe PlayerId
playerUUIDToPlayerId gameState uuid =
  M.lookup uuid (playerUUIDMap gameState)

playerIdToPlayerUUID :: GameState -> PlayerId -> Maybe PlayerUUID
playerIdToPlayerUUID gameState pid =
  M.lookup pid (playerIdMap gameState)

playerUUIDMap :: GameState -> M.Map PlayerUUID PlayerId
playerUUIDMap gameState = M.fromList
      $ S.elems
      $ S.map (\p -> (_uuid p, _id p))
      $ _players gameState

playerIdMap :: GameState -> M.Map PlayerId PlayerUUID
playerIdMap gameState = M.fromList
      $ S.elems
      $ S.map (\p -> (_id p, _uuid p))
      $ _players gameState

filterGameState :: Player -> GameState -> FilteredGameState
filterGameState player gameState = FilteredGameState
  { _fmode  = _mode gameState
  , _fteams = _teams gameState
  , _fcards = filterCard (_players gameState) player
  , _fpegs  = allPegs (_players gameState)
  }
  where
    allPegs :: S.Set Player -> M.Map PlayerId [Peg]
    allPegs players = M.fromList
      $ map (\q -> (_id q, _pegs q))
      $ S.elems
      $ players

    filterCard :: S.Set Player -> Player -> M.Map PlayerId [Visibility Card]
    filterCard players p = M.fromList
      $ map (\q -> ((_id q), (map (\c -> if _id p == _id q then setVisible c else setHidden c) (_cards p))))
      $ S.elems
      $ players

isOver :: GameState -> Bool
isOver gameState =
  maybe False (const True) (winner gameState)

teamWon :: M.Map PlayerId Bool -> Team -> Bool
teamWon playerIdVictory (Team p1 p2) =
  fromMaybe False $ do
    victoryP1 <- M.lookup p1 playerIdVictory
    victoryP2 <- M.lookup p2 playerIdVictory
    return $ victoryP1 && victoryP2

winner :: GameState -> Maybe Team
winner gameState =
  case ( teamWon playerIdVictory team1
       , teamWon playerIdVictory team2
       ) of
    (True, False) -> return team1
    (False, True) -> return team2
    _             -> Nothing

  where
    (team1, team2) = _teams gameState

    playerIdVictory :: M.Map PlayerId Bool
    playerIdVictory = M.fromList
      $ map (\player -> (_id player, playerWon player))
      $ S.elems
      $ _players gameState

playerWon :: Player -> Bool
playerWon = all onTarget . _pegs

dealCards :: Deck -> Maybe (Deck, [Hand])
dealCards deck =
  if length deck < 4 * 4
  then Nothing
  else Just (deck', [h1, h2, h3, h4])

  where
    h1, h2, h3, h4 :: Hand
    h1 = take 4 deck
    h2 = take 4 (drop 4 deck)
    h3 = take 4 (drop 8 deck)
    h4 = take 4 (drop 12 deck)

    deck' :: Deck
    deck' = drop 16 deck

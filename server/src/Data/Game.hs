{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- TODO: move some types to Internal
--        * Make it available for testing
--        * Does not export some constructors
-- TODO: add doc for each exported function

module Data.Game
  ( Mode (..)
  , GameLog
  , GameError (..)

  , GameEnvironment (..)
  , defaultGameEnvironment

  , GameResult (..)
  , partner
  , playerTeam
  , nextTeam
  , nextPlayer

  , PlayerUUID
  , PlayerId (..)
  , Player (..)
  , Team (..)

  , Action (..)
  , PlayerAction (..)
  , NoPlayerAction (..)
  , emptyHand
  , defaultPegs
  , mkPlayer

  , Position (..)

  , BoardPosition (..)
  , boardPosition
  , startPosition
  , endPosition
  , runBoardPosition

  , TargetPosition (..)
  , targetPosition

  , Peg (..)
  , PegMode (..)
  , onTarget
  , onBoard
  , pegMode

  , Card (..)
  , Deck
  , Hand
  , playerIdToInt
  , intToPlayerId
  , intToCard
  , cardToInt
  , cardToMove
  , startCard
  , defaultDeck
  , emptyDeck
  , mkNextPlayerId

  , Visibility (..)
  , setVisible
  , setHidden

  , FilteredGameState (..)
  , filterGameState

  , GameState (..)
  , emptyGameState
  , isOver
  , winner
  , playerWon

  , findPlayer
  , findPlayerByPlayerId
  , getPlayerUUIDs
  , playerUUIDToPlayerId
  , playerIdToPlayerUUID
  , playerUUIDMap
  , playerIdMap

  , dealCards
  )
  where

import qualified Data.Char            as C
import           Data.Foldable        (maximum)
import           Data.Function        (on)
import qualified Data.List            as L
import qualified Data.Map             as M
import           Data.Maybe           (fromMaybe, isJust, maybe)
import           Data.Scientific      (toBoundedInteger)
import qualified Data.Set             as S
import qualified Data.Text            as T
import           Data.UUID            (UUID)
import           GHC.Generics         (Generic)

import           Control.Lens         (makeLenses)
import           Control.Monad.Random (MonadRandom, StdGen, getRandom)
import           Data.Aeson
import qualified Data.Aeson.Encoding  as AE
import qualified Data.Vector          as V

-- |Game `Mode` for a Trik Trak Game
data Mode
  = JoinWait                       -- ^ Waiting for other players to join
  | CardExchange                   -- ^ Exchanging cards between teammates
  | Winner { modeTeam :: !Team }    -- ^ End of the game with a winner team
  | Play { modePid :: !PlayerId }   -- ^ Regular round play with PlayerId playing
  deriving (Eq, Show, Generic)

instance ToJSON Mode where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = map C.toLower . drop 4 }

instance FromJSON Mode where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = map C.toLower . drop 4 }

-- |Game Log type for logging in the `GameMonad` type
type GameLog = [String]

-- |Environment
data GameEnvironment = GameEnvironment
  { _geTeams    :: !(Team, Team)  -- ^ Player Teams
  , _geNPlayers :: !Int        -- ^ N Players to join before the game can start
  }
  deriving (Show, Eq)

partner :: PlayerId -> PlayerId
partner P1 = P3
partner P3 = P1
partner P2 = P4
partner P4 = P2

playerTeam :: PlayerId -> Team
playerTeam P1 = Team P1 P3
playerTeam P3 = Team P1 P3
playerTeam P2 = Team P2 P4
playerTeam P4 = Team P2 P4

nextTeam :: Team -> Team
nextTeam (Team P1 P3) = Team P2 P4
nextTeam _            = Team P1 P3

nextPlayer :: PlayerId -> PlayerId
nextPlayer P1 = P2
nextPlayer P2 = P3
nextPlayer P3 = P4
nextPlayer P4 = P1

defaultGameEnvironment :: GameEnvironment
defaultGameEnvironment =
  GameEnvironment
    { _geTeams    = (Team P1 P3, Team P2 P4)
    , _geNPlayers = 4
    }

-- |Error that can be thrown in a `GameMonad` action
data GameError
  = InvalidAction Action                  -- ^ InvalidAction for given Action and Player
  | JoinTooManyPlayers                    -- ^ Too many players are already in the Game
  | WrongNumberPlayers { errorNPlayers :: !Int }                -- ^ Wrong Number of players
  | Unauthorized { errorPuuid :: !PlayerUUID }               -- ^ Unauthorized UUID
  | WrongPlayerTurn { errorPuuid :: !PlayerUUID }           -- ^ Wrong player turn
  | CardNotAvailabe { errorPuuid :: !PlayerUUID, errorCard :: !Card }       -- ^ Card is not availabe for player
  | PlayerNotFound { errorPuuid :: !PlayerUUID }             -- ^ Player Not Found exception
  | PlayerIdNotFound { errorPid :: !PlayerId }             -- ^ Player Not Found exception
  | CardAlreadyExchanged {errorCard :: !Card, errorPuuid :: !PlayerUUID }  -- ^ Card is already exchanged for PlayerUUID
  deriving (Eq, Show, Generic)

instance ToJSON GameError where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 5 }

data GameResult
  = NewPlayer  -- ^ NewPlayer has been created
    { gameResultUuid :: !PlayerUUID
    , gameResultPid  :: !PlayerId
    }
  | Unit       -- ^ Success but nothing to return
  deriving (Eq, Show, Generic)

instance ToJSON GameResult where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 10
  }

instance FromJSON GameResult where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 10
  }

type PlayerUUID = UUID

data Action
  = PA !Player !PlayerAction
  | NPA !NoPlayerAction
  deriving (Eq, Show, Generic)

instance ToJSON Action where
  toJSON (PA player action) = object
    [ "pid"    .= _pid player
    , "action" .= action
    ]

-- TODO: rename pA to pa
data PlayerAction
  = Exchange { pACard :: !Card }           -- ^ Exchange Card to PlayerId
  | Move  -- ^ Move Peg with Card to new Position
    { pACard     :: !Card
    , pAPeg      :: !Peg
    , pAPosition :: !Position
    }
  | Discard  -- ^ Discard Card
    { pACard :: !Card }
  | SwitchPegs -- ^ Switch two pegs inclluding Player Id
    { pAPid  :: !PlayerId
    , pAFrom :: !Peg
    , pATo   :: !Peg
    }
  | QuitGame                -- ^ Quit the game
  deriving (Eq, Show, Generic)

-- TODO: finish ToJSON
instance ToJSON PlayerAction where
  toJSON = genericToJSON defaultOptions {
      fieldLabelModifier = map C.toLower . drop 2
    }

instance FromJSON PlayerAction where
  parseJSON = genericParseJSON defaultOptions {
      fieldLabelModifier = map C.toLower . drop 2
    }

data NoPlayerAction
  = JoinGame      -- ^ Join the Game
  deriving (Eq, Show)

instance ToJSON NoPlayerAction where
  toJSON JoinGame = toJSON $ show JoinGame

-- |Representing a Player in the Game
data Player = Player
  { _puuid  :: !PlayerUUID -- ^ Player uuid
  , _pid    :: !PlayerId   -- ^ Player id
  , _pcards :: !Hand       -- ^ Hand of cards
  , _ppegs  :: ![Peg]      -- ^ Pegs
  }
  deriving (Eq, Show)

instance Ord Player where
  compare = compare `on` _pid

emptyHand :: Hand
emptyHand = []

defaultPegs :: [Peg]
defaultPegs = L.replicate 4 PegHome

mkPlayer :: MonadRandom m => PlayerId -> Hand -> [Peg] -> m Player
mkPlayer pid hand pegs = do
  uuid <- getRandom
  return $ Player uuid pid hand pegs

-- |Mode of a Peg
data PegMode
  = Normal  -- ^ Normal Mode
  | Stake   -- ^ Stake Mode
  deriving (Eq, Show)

instance ToJSON PegMode where
  toJSON mode = object
    [ "mode" .= T.pack (show mode)
    ]

instance FromJSON PegMode where
  parseJSON = withObject "PegMode" $ \o -> do
    modeType :: String <- o .: "mode"
    case modeType of
      "Normal" -> return Normal
      "Stake"  -> return Stake
      _        -> fail "Error parsing PegMode"

-- |Position on Board or Target
data Position
  = BP { pBPosition :: BoardPosition }
  | TP { pTPosition :: TargetPosition }
  deriving (Eq, Show, Ord, Generic)

instance ToJSON Position where
  toJSON = genericToJSON defaultOptions {
      fieldLabelModifier = map C.toLower . drop 2
    }

instance FromJSON Position where
  parseJSON = genericParseJSON defaultOptions {
      fieldLabelModifier = map C.toLower . drop 2
    }

newtype BoardPosition
  = BoardPosition Int  -- ^ Position on the Board
  deriving (Eq, Show, Ord)

startPosition :: BoardPosition
startPosition = BoardPosition 0

endPosition :: BoardPosition
endPosition = BoardPosition $ 4 * 16 - 1

runBoardPosition :: BoardPosition -> Int
runBoardPosition (BoardPosition x) = x

instance FromJSON BoardPosition where
  parseJSON = withScientific "BoardPosition" $ \n ->
    maybe (fail "Error Parsing BoardPosition") return $ do
      i <- toBoundedInteger n
      boardPosition i

instance ToJSON BoardPosition where
  toJSON (BoardPosition x) = toJSON x

newtype TargetPosition
  = TargetPosition Int  -- ^ Position on the Target
  deriving (Eq, Show, Ord)

instance ToJSON TargetPosition where
  toJSON (TargetPosition x) = toJSON x

instance FromJSON TargetPosition where
  parseJSON = withScientific "TargetPosition" $ \n ->
    maybe (fail "Error Parsing TargetPosition") return $ do
      i <- toBoundedInteger n
      targetPosition i

boardPosition :: Int -> Maybe BoardPosition
boardPosition n
  | n >= 16 * 4 = Nothing
  | n < 0 = Nothing
  | otherwise = Just $ BoardPosition n

targetPosition :: Int -> Maybe TargetPosition
targetPosition n
  | n >= 4 = Nothing
  | n < 0 = Nothing
  | otherwise = Just $ TargetPosition n

data Peg
  = PegBoard   -- ^ On the board at position with mode
    { pegBoardPosition :: !BoardPosition
    , pegPegMode       :: !PegMode
    }
  | PegTarget  -- ^ On the target at position
    { pegTargetPosition :: !TargetPosition }
  | PegHome    -- ^ Home
  deriving (Eq, Show, Generic)

instance ToJSON Peg where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 3
  }

instance FromJSON Peg where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 3
  }

onTarget :: Peg -> Bool
onTarget (PegBoard _ _) = False
onTarget _              = False

onBoard :: Peg -> Bool
onBoard (PegBoard _ _) = True
onBoard _              = False

pegMode :: Peg -> PegMode
pegMode PegHome           = Normal
pegMode (PegTarget _)     = Stake
pegMode (PegBoard _ mode) = mode

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
  | Switch
  | Twelve
  deriving (Eq, Show, Ord)

instance ToJSON Card where
  toJSON = toJSON . cardToInt

instance FromJSON Card where
  parseJSON = withScientific "Card" $ \n ->
    maybe (fail "Error") return $ do
      i <- toBoundedInteger n
      intToCard i

type Deck       = [Card]
type Hand       = [Card]
data PlayerId = P1 | P2 | P3 | P4
  deriving (Eq, Show, Ord)

instance FromJSON PlayerId where
  parseJSON = withScientific "PlayerId" $ \n ->
    maybe (fail "Error parsing PlayerId") return $ do
      i   <- toBoundedInteger n
      intToPlayerId i

data Team = Team !PlayerId !PlayerId
  deriving (Eq, Show, Ord)

instance ToJSON Team where
  toJSON (Team pid1 pid2) = toJSON (pid1, pid2)

instance FromJSON Team where
  parseJSON = withArray "Team" $ \a ->
    Team <$> parseJSON (a V.! 0) <*> parseJSON (a V.! 1)

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

intToCard :: Int -> Maybe Card
intToCard 1  = Just One
intToCard 2  = Just Two
intToCard 3  = Just Three
intToCard 4  = Just Four
intToCard 5  = Just Five
intToCard 6  = Just Six
intToCard 7  = Just Seven
intToCard 8  = Just Eight
intToCard 9  = Just Nine
intToCard 10 = Just Ten
intToCard 11 = Just Switch
intToCard 12 = Just Twelve
intToCard _  = Nothing

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

-- TODO: use Maybe Int instead to signal that switch does not move pegs
cardToMove :: Card -> Int
cardToMove One    = 1
cardToMove Two    = 2
cardToMove Three  = 3
cardToMove Four   = -4
cardToMove Five   = 5
cardToMove Six    = 6
cardToMove Seven  = 7
cardToMove Eight  = 8
cardToMove Nine   = 9
cardToMove Ten    = 10
cardToMove Switch = 0
cardToMove Twelve = 12

startCard :: Card -> Bool
startCard One = True
startCard Ten = True
startCard _   = False

defaultDeck :: Deck
defaultDeck =
    L.concat
    [ L.replicate 4 c | c <-
        [ One, Two, Three, Four
        , Five, Six, Seven, Eight
        , Nine, Ten, Switch, Twelve
        ]
    ]

emptyDeck :: Deck
emptyDeck = []

mkNextPlayerId :: S.Set Player -> Maybe PlayerId
mkNextPlayerId xs
  | S.null xs = Just P1
  | otherwise = intToPlayerId
      $ inc
      $ maximum
      $ fmap playerIdToInt
      $ S.toList
      $ S.map _pid xs
  where
    inc :: Int -> Int
    inc = (+1)

data GameState = GameState
  { _gstPlayers         :: !(S.Set Player)         -- ^ Set of players in the game
  , _gstRoundPlayerTurn :: !PlayerId               -- ^ Round Player Turn
  , _gstDeck            :: !Deck                   -- ^ Current deck of card
  , _gstMode            :: !Mode                   -- ^ Mode of the Game
  , _gstCardExchange    :: !(M.Map PlayerId Card)  -- ^ PlayerId exchanges Card
  , _gstGen             :: !StdGen                 -- ^ StdGen
  }
  deriving (Show)

makeLenses ''GameState

emptyGameState :: StdGen -> GameState
emptyGameState g = GameState
  { _gstMode            = JoinWait
  , _gstPlayers         = S.empty
  , _gstRoundPlayerTurn = P1
  , _gstDeck            = emptyDeck
  , _gstCardExchange    = M.empty
  , _gstGen             = g
  }

-- TODO: Do not export constructors
data Visibility a
  = Visible { visibilityValue :: !a }    -- ^ Visible Element
  | Hidden                               -- ^ Hidden Element
  deriving (Eq, Show, Ord, Generic)

setVisible :: a -> Visibility a
setVisible = Visible

setHidden :: a -> Visibility a
setHidden = const Hidden

instance ToJSON a => ToJSON (Visibility a) where
  toJSON = genericToJSON defaultOptions {
      fieldLabelModifier = camelTo2 '_' . drop 10
    }

instance FromJSON a => FromJSON (Visibility a) where
  parseJSON = genericParseJSON defaultOptions {
      fieldLabelModifier = camelTo2 '_' . drop 10
    }

-- TODO: prefix with `fgs`
-- | State of the game seen by a given player
data FilteredGameState = FilteredGameState
  { _fgstMode    :: !Mode                             -- ^ Game Mode
  , _fgstTeams   :: !(Team, Team)                     -- ^ Teams
  , _fgstCards   :: ![(PlayerId, [Visibility Card])]  -- ^ Player's cards
  , _fgstPegs    :: ![(PlayerId, [Peg])]              -- ^ Player's peg
  , _fgstActions :: ![PlayerAction]                   -- ^ Actions that can be performed
  , _fgstHistory :: ![PlayerAction]                   -- ^ History of all Actions performed so far
  }
  deriving (Eq, Show, Generic)

makeLenses ''FilteredGameState

instance ToJSON FilteredGameState where
  toJSON = genericToJSON defaultOptions {
      fieldLabelModifier = map C.toLower . drop 2
    }

instance FromJSON FilteredGameState where
  parseJSON = genericParseJSON defaultOptions {
      fieldLabelModifier = map C.toLower . drop 2
    }

findPlayer :: GameState -> PlayerUUID -> Maybe Player
findPlayer s uuid = L.find ((== uuid) . _puuid) players
  where
    players :: [Player]
    players = S.elems (_gstPlayers s)

findPlayerByPlayerId :: GameState -> PlayerId -> Maybe Player
findPlayerByPlayerId s pid = L.find ((== pid) . _pid) players
  where
    players :: [Player]
    players = S.elems (_gstPlayers s)

getPlayerUUIDs :: GameState -> S.Set PlayerUUID
getPlayerUUIDs = S.map _puuid . _gstPlayers

playerUUIDToPlayerId :: GameState -> PlayerUUID -> Maybe PlayerId
playerUUIDToPlayerId gameState uuid =
  M.lookup uuid (playerUUIDMap gameState)

playerIdToPlayerUUID :: GameState -> PlayerId -> Maybe PlayerUUID
playerIdToPlayerUUID gameState pid =
  M.lookup pid (playerIdMap gameState)

playerUUIDMap :: GameState -> M.Map PlayerUUID PlayerId
playerUUIDMap gameState = M.fromList
      $ S.elems
      $ S.map (\p -> (_puuid p, _pid p))
      $ _gstPlayers gameState

playerIdMap :: GameState -> M.Map PlayerId PlayerUUID
playerIdMap gameState = M.fromList
      $ S.elems
      $ S.map (\p -> (_pid p, _puuid p))
      $ _gstPlayers gameState

filterGameState :: Player -> GameState -> GameEnvironment -> FilteredGameState
filterGameState player gameState gameEnv = FilteredGameState
  { _fgstMode    = _gstMode gameState
  , _fgstTeams   = _geTeams gameEnv
  , _fgstCards   = filterCard (_gstPlayers gameState) player
  , _fgstPegs    = allPegs (_gstPlayers gameState)
  , _fgstActions = []
  , _fgstHistory = []
  }
  where
    allPegs :: S.Set Player -> [(PlayerId, [Peg])]
    allPegs players = map (\q -> (_pid q, _ppegs q))
      $ S.elems players

    filterCard :: S.Set Player -> Player -> [(PlayerId, [Visibility Card])]
    filterCard players p = map (\q -> (_pid q, map (\c -> if _pid p == _pid q then setVisible c else setHidden c) (_pcards p)))
      $ S.elems players
      -- TODO: use lens to simplify

isOver :: GameEnvironment -> GameState -> Bool
isOver gameEnv gameState = isJust $ winner gameEnv gameState

teamWon :: M.Map PlayerId Bool -> Team -> Bool
teamWon playerIdVictory (Team p1 p2) =
  fromMaybe False $ do
    victoryP1 <- M.lookup p1 playerIdVictory
    victoryP2 <- M.lookup p2 playerIdVictory
    return $ victoryP1 && victoryP2

winner :: GameEnvironment -> GameState -> Maybe Team
winner gameEnv gameState =
  case ( teamWon playerIdVictory team1
       , teamWon playerIdVictory team2
       ) of
    (True, False) -> return team1
    (False, True) -> return team2
    _             -> Nothing

  where
    (team1, team2) = _geTeams gameEnv

    playerIdVictory :: M.Map PlayerId Bool
    playerIdVictory = M.fromList
      $ map (\player -> (_pid player, playerWon player))
      $ S.elems
      $ _gstPlayers gameState

playerWon :: Player -> Bool
playerWon = all onTarget . _ppegs

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

{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Game where

import           Control.Monad.Random (MonadRandom, StdGen, getRandom)
import           Data.Aeson
import qualified Data.Aeson.Encoding  as AE
import           Data.Aeson.Types
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
import qualified Data.Vector          as V
import           GHC.Generics         (Generic)

-- |Game `Mode` for a Trik Trak Game
data Mode
  = JoinWait                       -- ^ Waiting for other players to join
  | CardExchange                   -- ^ Exchanging cards between teammates
  | Winner { modeTeam :: Team }    -- ^ End of the game with a winner team
  | Play { modePid :: PlayerId }   -- ^ Regular round play with PlayerId playing
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
newtype GameEnvironment = GameEnvironment
  { teams :: (Team, Team)  -- ^ Player Teams
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
defaultGameEnvironment = GameEnvironment (Team P1 P3, Team P2 P4)

-- |Error that can be thrown in a `GameMonad` action
data GameError
  = InvalidAction Action                  -- ^ InvalidAction for given Action and Player
  | JoinTooManyPlayers                    -- ^ Too many players are already in the Game
  | WrongNumberPlayers { errorNPlayers :: Int }                -- ^ Wrong Number of players
  | Unauthorized { errorPid :: PlayerUUID }               -- ^ Unauthorized UUID
  | WrongPlayerTurn { errorPid :: PlayerUUID }           -- ^ Wrong player turn
  | CardNotAvailabe { errorPid :: PlayerUUID, errorCard :: Card }       -- ^ Card is not availabe for player
  | PlayerNotFound { errorPid :: PlayerUUID }             -- ^ Player Not Found exception
  | CardAlreadyExchanged {errorCard :: Card, errorPid :: PlayerUUID }  -- ^ Card is already exchanged for PlayerUUID
  deriving (Eq, Show, Generic)

instance ToJSON GameError where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 5 }

-- instance ToJSON GameError where
--   toJSON (InvalidAction (PA p _)) = object
--     [ "type"     .= ("InvalidAction" :: T.Text)
--     , "player_id" .= _id p
--     ]
--   toJSON JoinTooManyPlayers = object
--     [ "type" .= ("JoinTooManyPlayers" :: T.Text)]
--   toJSON (Unauthorized uuid) = object
--     [ "type" .= ("Unauthorized for uuid: " ++ show uuid) ]
--   toJSON (WrongPlayerTurn uuid) = object
--     [ "type" .= ("WrongPlayerTurn for uuid: " ++ show uuid) ]
--   toJSON (CardNotAvailabe uuid card) = object
--     [ "type" .= ("CardNotAvailabe " ++ show card ++ " for player " ++ show uuid) ]
--   toJSON (PlayerNotFound uuid) = object
--     [ "type" .= ("PlayerNotFound for uuid: " ++ show uuid) ]
--   toJSON (CardAlreadyExchanged card uuid) = object
--     [ "type" .= ("CardAlreadyExchanged " ++ show card ++ " for player " ++ show uuid) ]

-- TODO: add a FilteredGameState
data GameResult
  = NewPlayer PlayerUUID PlayerId
  | Unit
  deriving (Eq, Show)

instance ToJSON GameResult where
  toJSON (NewPlayer uuid pid) = object
    [ "type" .= ("NewPlayer" :: T.Text)
    , "uuid" .= uuid
    , "pid"  .= pid
    ]
  toJSON Unit = object
    [ "type" .= ("Unit" :: T.Text) ]

-- TODO:
instance FromJSON GameResult where
  parseJSON = withObject "GameResult" $ \o -> do
    (resultType :: T.Text) <- o .: "type"
    case resultType of
      "NewPlayer" -> parseNewPlayer o
      "Unit"      -> return Unit
      _           -> fail "Error parsing GameResult"
    where
      parseNewPlayer o = NewPlayer <$> o .: "uuid" <*> o .: "pid"

type PlayerUUID = UUID  -- ^ Type synonym for Player UUID

data Action
  = PA Player PlayerAction
  | NPA NoPlayerAction
  deriving (Eq, Show)

instance ToJSON Action where
  toJSON (PA player action) = object
    [ "pid"    .= _id player
    , "action" .= action
    ]

data PlayerAction
  = Exchange Card           -- ^ Exchange Card to PlayerId
  | Move Card Peg Position  -- ^ Move Peg with Card to new Position
  | Discard Card            -- ^ Discard Card
  | SwitchPegs PlayerId Peg Peg   -- ^ Switch two pegs inclluding Player Id
  | QuitGame                -- ^ Quit the game
  deriving (Eq, Show)

-- TODO: finish ToJSON
-- TODO: activate Warnings for incomplete pattern matching
instance ToJSON PlayerAction where
  toJSON (Exchange c) = object
    [ "type" .= ("Exchange" :: T.Text)
    , "card" .= c
    ]
  toJSON (Move c peg pos) = object
    [ "type"      .= ("Move" :: T.Text)
    , "card"      .= c
    , "peg"       .= peg
    , "position"  .= pos
    ]

instance FromJSON PlayerAction where
  parseJSON = withObject "PlayerAction" $ \o -> do
    actionType :: String <- o .: "type"
    case actionType of
      "Exchange"   -> parseExchange o
      "Move"       -> parseMove o
      "Discard"    -> parseDiscard o
      "SwitchPegs" -> parseSwitchPegs o
      "QuitGame"   -> return QuitGame
      _            -> fail "Error Parsing PlayerAction"

    where
      parseExchange   o = Exchange <$> o .: "card"
      parseDiscard    o = Discard <$> o .: "card"
      parseSwitchPegs o = SwitchPegs <$> o .: "pid" <*> o .: "from" <*> o .: "to"
      parseMove       o = Move <$> o .: "card" <*> o .: "peg" <*> o .: "position"

data NoPlayerAction
  = JoinGame      -- ^ Join the Game
  deriving (Eq, Show)

instance ToJSON NoPlayerAction where
  toJSON JoinGame = toJSON $ show JoinGame

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
defaultPegs = L.replicate 4 PegHome

mkPlayer :: MonadRandom m => PlayerId -> Hand -> [Peg] -> m Player
mkPlayer pid hand pegs = do
  uuid <- getRandom
  return $ Player uuid pid hand pegs

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

data Position
  = BP BoardPosition
  | TP TargetPosition
  deriving (Eq, Show, Ord)

instance FromJSON Position where
  parseJSON = withObject "Position" $ \o -> do
    positionType :: String <- o .: "type"
    position <- o .: "position"
    case positionType of
      "Board" ->
        case boardPosition position of
          Nothing -> fail "Error Parsing BoardPosition"
          Just bp -> return (BP bp)
      "Target" ->
        case targetPosition position of
          Nothing -> fail "Error Parsing TargetPosition"
          Just tp -> return (TP tp)

instance ToJSON Position where
  toJSON (BP bp) = object
    [ "type" .= ("Board" :: T.Text)
    , "position" .= bp
    ]
  toJSON (TP tp) = object
    [ "type" .= ("Target" :: T.Text)
    , "position" .= tp
    ]

newtype BoardPosition
  = BoardPosition Int  -- ^ Position on the Board
  deriving (Eq, Show, Ord)

startPosition :: BoardPosition
startPosition = BoardPosition 0

endPosition :: BoardPosition
endPosition = BoardPosition $ 4 * 16 - 1

-- TODO: remove
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
  = PegBoard BoardPosition PegMode   -- ^ On the board at position with mode
  | PegTarget TargetPosition         -- ^ On the target at position
  | PegHome                          -- ^ Home
  deriving (Eq, Show)

instance FromJSON Peg where
  parseJSON = withObject "Peg" $ \o -> do
    location :: String <- o .: "location"
    case location of
      "Home"   -> return PegHome
      "Target" -> parsePegTarget o
      "Board"  -> parsePegBoard o
      _        -> fail "Error parsing Peg"
    where
      parsePegTarget o = PegTarget <$> o .: "position"
      parsePegBoard o = PegBoard <$> o .: "position" <*> o .: "mode"

instance ToJSON Peg where
  toJSON PegHome = object
    [ "location" .= T.pack "Home"
    ]
  toJSON (PegTarget (TargetPosition x)) = object
    [ "location" .= T.pack "Target"
    , "position" .= x
    ]
  toJSON (PegBoard (BoardPosition x) mode) = object
    [ "location" .= T.pack "Board"
    , "position" .= x
    , "stake" .= (mode == Stake :: Bool)
    ]

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

data Team = Team PlayerId PlayerId
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
      $ S.map _id xs
  where
    inc :: Int -> Int
    inc = (+1)

data GameState = GameState
  { _players         :: S.Set Player      -- ^ Set of players in the game
  , _roundPlayerTurn :: PlayerId -- ^ Round Player Turn
  , _deck            :: Deck              -- ^ Current deck of card
  , _mode            :: Mode              -- ^ Mode of the Game
  , _teams           :: (Team, Team)      -- ^ Teams of the game
  -- TODO: use a type to abstract over M.Map PlayerId Card
  , _cardExchange    :: M.Map PlayerId Card  -- ^ PlayerId exchanges Card
  , _gen             :: StdGen              -- ^ StdGen
  }
  deriving (Show)

emptyGameState :: StdGen -> GameState
emptyGameState g = GameState
  { _mode         = JoinWait
  , _players      = S.empty
  , _roundPlayerTurn = P1
  , _deck         = emptyDeck
  , _teams        = (Team P1 P3, Team P2 P4)
  , _cardExchange = M.empty
  , _gen          = g
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
  { _fmode    :: Mode                              -- ^ Game Mode
  , _fteams   :: (Team, Team)                      -- ^ Teams
  , _fcards   :: M.Map PlayerId [Visibility Card]  -- ^ Player's cards
  , _fpegs    :: M.Map PlayerId [Peg]              -- ^ Player's pegs
  , _factions :: [PlayerAction]                  -- ^ Actions that can be performed
  , _fhistory :: [Action]                         -- ^ History of all Actions performed so far
  }
  deriving (Eq, Show)

instance ToJSON FilteredGameState where
  toJSON s = object
    [ "mode"   .= _fmode s
    , "teams"  .= _fteams s
    , "cards"  .= _fcards s
    , "pegs"   .= _fpegs s
    , "actions".= _factions s
    , "history".= _fhistory s
    ]

instance FromJSON FilteredGameState where
  parseJSON = withObject "FilteredGameState" $ \o -> do
    mode <- o .: "mode"
    teams <- o .: "teams"
    -- cards <- o .: "cards"
    actions <- o .: "actions"
    -- history <- o .: "history"
    return $ FilteredGameState mode teams M.empty M.empty actions []
    -- FilteredGameState <$>
    --   o .: "mode" <*>
    --   o .: "teams" <*>
    --   o .: "cards" <*>
    --   o .: "pegs" <*>
    --   o .: "actions" <*>
    --   o .: "history"

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
  , _factions = []
  , _fhistory = []
  }
  where
    allPegs :: S.Set Player -> M.Map PlayerId [Peg]
    allPegs players = M.fromList
      $ map (\q -> (_id q, _pegs q))
      $ S.elems players

    filterCard :: S.Set Player -> Player -> M.Map PlayerId [Visibility Card]
    filterCard players p = M.fromList
      $ map (\q -> (_id q, map (\c -> if _id p == _id q then setVisible c else setHidden c) (_cards p)))
      $ S.elems players
      -- TODO: use lens to simplify

isOver :: GameState -> Bool
isOver gameState = isJust $ winner gameState

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

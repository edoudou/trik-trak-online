{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- TODO: add export list
module Game where

import           Control.Monad         (guard, when)
import           Control.Monad.Except  (throwError)
import           Control.Monad.Reader  (ask)
import           Control.Monad.State   (get, gets)
import           Control.Monad.Writer  (tell)
import           Data.Function         (on)
import qualified Data.List             as L
import qualified Data.Map              as M
import           Data.Maybe            (maybeToList)
import qualified Data.Set              as S
import           Data.String           (unlines)

import           Control.Lens          (mapped, over, set, use, view, (%=),
                                        (.=))
import           System.Random.Shuffle (shuffleM)


import           Data.Game             hiding (pid)
import           GameMonad             (GameMonad)


handle :: Action -> GameMonad GameResult
handle (NPA JoinGame) = do
  (uuid, pid)       <- join
  isGameReadyToPlay <- readyToPlay
  when isGameReadyToPlay initGame
  return $ NewPlayer uuid pid

handle (PA player playerAction@(Exchange card)) =
  withValidPlayerAction player playerAction $
    withCardNotAlreadyExchanged player $ do
      exchangeCard player card
      return Unit

handle (PA player playerAction@(Move card peg position)) =
  withValidPlayerAction player playerAction $ do
    move player peg position
    useCard player card
    return Unit

handle (PA player playerAction@(SwitchPegs pid peg1 peg2)) =
  withValidPlayerAction player playerAction $ do
    performSwitchPegs (player, peg1) (pid, peg2)
    useCard player Switch
    return Unit

handle (PA player playerAction@(Discard card)) =
  withValidPlayerAction player playerAction $ do
    useCard player card
    return Unit

handle (PA player QuitGame) =
  withValidPlayerAction player QuitGame $ do
    quitGame player
    return Unit

performSwitchPegs :: (Player, Peg) -> (PlayerId, Peg) -> GameMonad ()
performSwitchPegs (player1, peg1) (pid2, peg2) = do
  gameState@GameState {..} <- get

  case findPlayerByPlayerId gameState pid2 of
    Nothing ->
      throwError $ PlayerIdNotFound pid2

    Just player2 ->
      let (newPlayer1, newPlayer2) = switchPegs (player1, peg1) (player2, peg2)
      in do
        gstPlayers %= (S.delete player1 . S.delete player2)
        gstPlayers %= (S.insert newPlayer1 . S.insert newPlayer2)

switchPegs :: (Player, Peg) -> (Player, Peg) -> (Player, Player)
switchPegs (player1, peg1) (player2, peg2) =
  ( over ppegs (\pegs -> peg2 : L.delete peg1 pegs) player1
  , over ppegs (\pegs -> peg1 : L.delete peg2 pegs) player2
  )

isValidPlayerAction :: GameState -> Player -> PlayerAction -> Bool
isValidPlayerAction GameState {..} Player {..} action =
  case _gstMode of
    Play pid     -> (pid == _pid) && action `L.elem` cardActions _gstPlayers _pid
    CardExchange -> action `L.elem` (Exchange <$> _pcards)
    _            -> False

withValidPlayerAction :: Player -> PlayerAction -> GameMonad a -> GameMonad a
withValidPlayerAction player playerAction action = do
  gameState <- get
  isValid   <- pure $ isValidPlayerAction gameState player playerAction
  if isValid
     then do
       tell ["Performing valid PlayerAction" ++ show playerAction]
       action
     else do
       tell ["Invalid PlayerAction: " ++ show playerAction]
       throwError $ InvalidAction (PA player playerAction)

-- Assumption: valid player action
move :: Player -> Peg -> Position -> GameMonad ()
move player peg position =
  case position of
    TP _  -> movePeg player peg position
    BP bp -> do
      pegEaten <- eatPeg bp
      when pegEaten $ tell ["Eating Peg located at this position: " ++ show bp]
      movePeg player peg position

-- Assumption: valid player action
movePeg :: Player -> Peg -> Position -> GameMonad ()
movePeg player@Player {..} peg position = do
  gstPlayers %= S.delete player
  gstPlayers %= S.insert (over ppegs (\pegs -> newPeg : L.delete peg pegs) player)

  where
    newPeg :: Peg
    newPeg =
      case position of
        BP bp -> PegBoard bp (if bp == startBoardPosition _pid then Stake else Normal)
        TP tp -> PegTarget tp

-- Assumption: valid player action
eatPeg :: BoardPosition -> GameMonad Bool
eatPeg bp = do
  s <- get
  gstPlayers %= \players -> S.fromList $ newPlayers $ S.elems players
  s' <- get
  return $ on (/=) _gstPlayers s s'

  where
    eatPegForPlayer :: Player -> Player
    eatPegForPlayer = over (ppegs . mapped) eatPeg'

    eatPeg' :: Peg -> Peg
    eatPeg' peg@(PegBoard bp' _) = if bp == bp' then PegHome else peg
    eatPeg' peg                  = peg

    newPlayers :: [Player] -> [Player]
    newPlayers ps = eatPegForPlayer <$> ps

withCardNotAlreadyExchanged :: Player -> GameMonad a -> GameMonad a
withCardNotAlreadyExchanged player action = do
  mCardExchanged <- hasAlreadyExchangedCard player
  case mCardExchanged of
    Just c' -> do
      tell ["Card already exchanged by player: " ++ show (_pid player)]
      throwError $ CardAlreadyExchanged c' (_puuid player)

    Nothing -> do
      tell ["Card not already exchanged by player: " ++ show (_pid player)]
      action

exchangeCard :: Player -> Card -> GameMonad ()
exchangeCard player card = do
  tell ["Giving Card " ++ show card ++ " to Player " ++ show (_pid player)]
  giveCard player card
  canExchangeAmongPlayers <- canExchangeCardsAmongPlayers
  when canExchangeAmongPlayers performCardExchange

-- TODO: rename to discardCard?
useCard :: Player -> Card -> GameMonad ()
useCard player card =
  gstPlayers %= \players -> S.fromList $ newPlayer : L.delete player (S.elems players)

  where
    newPlayer :: Player
    newPlayer = over pcards (L.delete card) player

-- TODO: add state for players who left
quitGame :: Player -> GameMonad ()
quitGame Player {..} =
  gstMode .= Winner winnerTeam
  where
    winnerTeam :: Team
    winnerTeam = nextTeam $ playerTeam _pid

cardAction :: S.Set Player -> PlayerId -> Peg -> Card -> [PlayerAction]
cardAction players pid peg card = moves ++ switches
  where
    switches :: [PlayerAction]
    switches = switch players pid card peg

    moves :: [PlayerAction]
    moves = Move card peg <$> filter (blockedByStakePeg players pid peg)
                                     (movePegWithCard pid card peg)

cardActions :: S.Set Player -> PlayerId -> [PlayerAction]
cardActions players pid =
  case playerActions of
    [] -> discards
    _  -> playerActions

  where
    cards :: [Card]
    cards = do
      player <- S.elems players
      guard (_pid player == pid)
      _pcards player

    playerActions :: [PlayerAction]
    playerActions = L.nub $ L.concat $ do
      player <- S.elems players
      guard (_pid player == pid)
      card   <- cards
      peg    <- _ppegs player
      return $ cardAction players pid peg card

    discards :: [PlayerAction]
    discards = Discard <$> cards

traversedPositions :: PlayerId -> Peg -> Position -> [Position]
traversedPositions pid PegHome to = BP (startBoardPosition pid) : traversedPositions pid (PegBoard (startBoardPosition pid) Stake) to
traversedPositions _ (PegBoard (BoardPosition from) _) (BP (BoardPosition to))
  | to >= from = [BP (BoardPosition x) | x <- [ from + 1 .. to ]]
  | to < from =
     [ BP (BoardPosition x) |
       x <- [ runBoardPosition startPosition .. to ]
            ++ [ from .. runBoardPosition endPosition ]
     ]
traversedPositions pid from@(PegBoard (BoardPosition _) _) to@(TP (TargetPosition _)) =
  traversedPositions pid from (BP (startBoardPosition pid))
  ++ [TP (TargetPosition 0)]
  ++ traversedPositions pid (PegTarget (TargetPosition 0)) to
traversedPositions _ (PegTarget (TargetPosition from)) (TP (TargetPosition to)) =
  [TP (TargetPosition x) | x <- [from + 1 .. to]]
traversedPositions _ _ _ = []

blockedByStakePeg :: S.Set Player -> PlayerId -> Peg -> Position -> Bool
blockedByStakePeg players pid peg to =
  (/= 0)
  $ S.size
  $ S.intersection traversedPos
  $ S.union blockedBoardPositions blockedTargetPositions

  where
    traversedPos :: S.Set Position
    traversedPos = S.fromList $ traversedPositions pid peg to

    blockedBoardPositions :: S.Set Position
    blockedBoardPositions = S.fromList $ do
      player <- S.elems players
      pg    <- _ppegs player
      case pg of
        PegBoard bp Stake -> return $ BP bp
        _                 -> []

    blockedTargetPositions :: S.Set Position
    blockedTargetPositions = S.fromList $ do
      player <- S.elems players
      guard (_pid player == pid)
      pg    <- _ppegs player
      case pg of
        PegTarget tp -> return $ TP tp
        _            -> []

movePegWithCard :: PlayerId -> Card -> Peg -> [Position]
movePegWithCard pid card PegHome
  | startCard card = return $ BP (startBoardPosition pid)
  | otherwise      = []
movePegWithCard _ card (PegTarget tp) = TP <$> maybeToList (moveTarget card tp)
movePegWithCard pid card (PegBoard bp _) =
  BP bp' : (TP <$> maybeToList tp')
  where
    bp' :: BoardPosition
    bp' = moveBoard card bp

    tp' :: Maybe TargetPosition
    tp' = moveFromBoardToTarget pid card bp

switch :: S.Set Player -> PlayerId -> Card -> Peg -> [PlayerAction]
switch players pid Switch peg@(PegBoard _ _) = do
  player <- S.elems players
  ppeg   <- _ppegs player
  guard (onBoard ppeg)
  guard (_pid player /= pid && pegMode ppeg == Stake)
  return $ SwitchPegs (_pid player) peg ppeg
switch _ _ _ _ = []

moveTarget :: Card -> TargetPosition -> Maybe TargetPosition
moveTarget card (TargetPosition x)
  | cardToMove card <= 0     = Nothing
  | cardToMove card + x <= 3 = Just $ TargetPosition (cardToMove card + x)
  | otherwise                = Nothing

moveBoard :: Card -> BoardPosition -> BoardPosition
moveBoard card (BoardPosition x) = BoardPosition $ cardToMove card + x

moveFromBoardToTarget :: PlayerId -> Card -> BoardPosition -> Maybe TargetPosition
moveFromBoardToTarget pid card (BoardPosition x)
  | cardToMove card <= 0                                = Nothing
  | targetPositionValue < 3 && targetPositionValue >= 0 = Just $ TargetPosition targetPositionValue
  | otherwise                                           = Nothing

  where
    (BoardPosition boardPositionStart) = startBoardPosition pid

    targetPositionValue :: Int
    targetPositionValue = x + cardToMove card - boardPositionStart

startBoardPosition :: PlayerId -> BoardPosition
startBoardPosition P1 = BoardPosition 0
startBoardPosition P2 = BoardPosition 16
startBoardPosition P3 = BoardPosition 32
startBoardPosition P4 = BoardPosition 48

hasAlreadyExchangedCard :: Player -> GameMonad (Maybe Card)
hasAlreadyExchangedCard Player {..} = gets $ M.lookup _pid . _gstCardExchange

canExchangeCardsAmongPlayers :: GameMonad Bool
canExchangeCardsAmongPlayers = do
  cardExchange <- use gstCardExchange
  nPlayers     <- view geNPlayers
  return $ length (M.keys cardExchange) == nPlayers

performCardExchange :: GameMonad ()
performCardExchange = do
  GameState {..} <- get
  tell ["Exchanging cards among players with the following cards: " ++ show _gstCardExchange]
  gstPlayers         %= flip exchangeCards' _gstCardExchange
  gstCardExchange    .= M.empty
  gstMode            .= Play (nextPlayer _gstRoundPlayerTurn)
  gstRoundPlayerTurn %= nextPlayer

  where
    exchangeCards' :: S.Set Player -> M.Map PlayerId Card -> S.Set Player
    exchangeCards' players cardsToExchange = S.fromList $ do
      player@Player {..} <- S.elems players
      case M.lookup (partner _pid) cardsToExchange of
        Nothing   -> return player
        Just card -> return player { _pcards = card : _pcards }

giveCard :: Player -> Card -> GameMonad ()
giveCard player card = do
  GameState {..} <- get

  case L.find (== card) (_pcards player) of
    Nothing ->
      throwError $ CardNotAvailabe (_puuid player) card

    Just _ ->
      let newHand = L.delete card (_pcards player)
       in do
         gstCardExchange %= M.insert (_pid player) card

         gstPlayers %=
           S.map (\p -> if _pid p == _pid player
                        then set pcards newHand p
                        else p)

showGameState :: GameState -> String
showGameState gameState = unlines
  [ "Mode: " ++ show (_gstMode gameState)
  , "Deck: " ++ show (_gstDeck gameState)
  , "CardExchange: " ++ show (_gstCardExchange gameState)
  , "Players: " ++ unlines ["  " ++ showPlayer p | p <- S.elems (_gstPlayers gameState)]
  ]
  where
    showPlayer :: Player -> String
    showPlayer p = unlines
      [ "PlayerId: " ++ show (_pid p)
      , "PlayerUUID: " ++ show (_puuid p)
      , "Hand: " ++ show (_pcards p)
      , "Pegs: " ++ show (_ppegs p)
      ]

checkPlayerNumber :: Int -> GameMonad ()
checkPlayerNumber n = do
  gameState <- get
  let nPlayers = length (_gstPlayers gameState)
  when (nPlayers /= n) $ throwError $ WrongNumberPlayers nPlayers

checkPlayerTurn :: Player -> GameMonad Bool
checkPlayerTurn player = do
  gameState <- get
  case _gstMode gameState of
    Play pid     -> return (pid == _pid player)  -- ^ Only the player with the assiged pid can play
    CardExchange -> return True                 -- ^ Anyone can exchange cards at the same time
    _            -> return False                -- ^ Not the player's turn

mkDeck :: GameMonad Deck
mkDeck = do
  deck    <- shuffleM defaultDeck
  gstDeck .= deck
  return deck

initGame :: GameMonad ()
initGame = mkDeck >> deal

deal :: GameMonad ()
deal = do
  GameState {..} <- get
  case dealCards _gstDeck of
    Nothing -> do
      tell ["Not enough cards in Deck, making a new Deck"]
      mkDeck
      deal    -- Deal again

    Just (deck', hands) -> do
      tell ["Dealing cards from deck"]
      gstPlayers %= giveHand hands
      gstMode .= CardExchange
      gstDeck .= deck'

  where
    giveHand :: [Hand] -> S.Set Player -> S.Set Player
    giveHand hands players =
      S.fromList [p { _pcards = h } | (h, p) <- zip hands (S.elems players)]


join :: GameMonad (PlayerUUID, PlayerId)
join = do
  GameState {..} <- get
  case mkNextPlayerId _gstPlayers of
    Nothing       -> do
      tell ["Cannot add a new player, the party is full"]
      throwError JoinTooManyPlayers
    Just playerId -> do
      player@Player {..} <- mkPlayer playerId emptyHand defaultPegs
      tell ["Adding new Player to the Game: " ++ show player]
      gstPlayers %= S.insert player
      return (_puuid, _pid)

readyToPlay :: GameMonad Bool
readyToPlay = do
  GameState       {..} <- get
  GameEnvironment {..} <- ask
  return $ length _gstPlayers == _geNPlayers

getState :: PlayerUUID -> GameMonad FilteredGameState
getState uuid = do
  gameState <- get
  gameEnv   <- ask
  tell ["[uuid: " ++ show uuid ++ "] Requesting GameState"]
  case findPlayer gameState uuid of
    Nothing     -> throwError $ Unauthorized uuid
    Just player -> return $ filterGameState player gameState gameEnv

{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- TODO: add export list
module Game where

import           Control.Monad         (guard, when)
import           Control.Monad.Except  (throwError)
import           Control.Monad.Reader  (ask)
import           Control.Monad.State   (get, gets, modify')
import           Control.Monad.Writer  (tell)
import           Data.Function         (on)
import qualified Data.List             as L
import qualified Data.Map              as M
import           Data.Maybe            (maybeToList)
import qualified Data.Set              as S
import           Data.String           (unlines)

import           System.Random.Shuffle (shuffleM)


import           Data.Game
import           GameMonad (GameMonad)


handle :: Action -> GameMonad GameResult
handle (NPA JoinGame) = do
  (uuid, pid)       <- join
  isGameReadyToPlay <- readyToPlay
  when isGameReadyToPlay initGame
  return (NewPlayer uuid pid)

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
  gameState <- get
  case findPlayerByPlayerId gameState pid2 of
    Nothing ->
      throwError $ PlayerIdNotFound pid2

    Just player2 ->
      let (newPlayer1, newPlayer2) = switchPegs (player1, peg1) (player2, peg2)
      in modify' (\s -> s { _players = S.insert newPlayer1 (S.insert newPlayer2 (S.delete player2 (S.delete player1 (_players s))))})
      -- TODO: use lens to simplify

switchPegs :: (Player, Peg) -> (Player, Peg) -> (Player, Player)
switchPegs (player1, peg1) (player2, peg2) =
  ( player1 { _pegs = peg2 : L.delete peg1 (_pegs player1)}
  , player2 { _pegs = peg1 : L.delete peg2 (_pegs player2)}
  )

isValidPlayerAction :: GameState -> Player -> PlayerAction -> Bool
isValidPlayerAction GameState {..} Player {..} action =
  case _mode of
    Play pid     -> (pid == _id) && action `L.elem` cardActions _players _id
    CardExchange -> action `L.elem` (Exchange <$> _cards)
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
-- TODO: use lens to simplify nested record updates
movePeg :: Player -> Peg -> Position -> GameMonad ()
movePeg player@Player {..} peg position =
  modify' (\s -> s { _players = S.insert newPlayer (S.delete player (_players s))})

  where
    newPeg :: Peg
    newPeg =
      case position of
        BP bp -> PegBoard bp (if bp == startBoardPosition _id then Stake else Normal)
        TP tp -> PegTarget tp

    newPegs :: [Peg]
    newPegs = newPeg : L.delete peg _pegs

    newPlayer :: Player
    newPlayer = player { _pegs = newPegs }


-- TODO: use lens to simplify
-- Assumption: valid player action
eatPeg :: BoardPosition -> GameMonad Bool
eatPeg bp = do
  gameState <- get
  modify' (\s -> s { _players = S.fromList (newPlayers (S.elems (_players s)))})
  gameState' <- get
  return $ on (/=) _players gameState gameState'

  where
    eatPegForPlayer :: Player -> Player
    eatPegForPlayer p = p { _pegs = eatPeg' <$> _pegs p }

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
      tell ["Card already exchanged by player: " ++ show (_id player)]
      throwError $ CardAlreadyExchanged c' (_uuid player)

    Nothing -> do
      tell ["Card not already exchanged by player: " ++ show (_id player)]
      action

exchangeCard :: Player -> Card -> GameMonad ()
exchangeCard player card = do
  tell ["Giving Card " ++ show card ++ " to Player " ++ show (_id player)]
  giveCard player card
  canExchangeAmongPlayers <- canExchangeCardsAmongPlayers
  when canExchangeAmongPlayers performCardExchange

-- TODO: use lens to simplify
-- TODO: rename to discardCard?
useCard :: Player -> Card -> GameMonad ()
useCard player card =
  modify' (\s -> s { _players = S.fromList (newPlayer : L.delete player (S.elems (_players s)))})

  where
    newPlayer :: Player
    newPlayer = player { _cards = L.delete card (_cards player) }

-- TODO: add state for players who left
quitGame :: Player -> GameMonad ()
quitGame player = do
  modify' (\s -> s { _mode = Winner winnerTeam })
  return ()

  where
    winnerTeam :: Team
    winnerTeam = nextTeam $ playerTeam $ _id player

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
      guard (_id player == pid)
      _cards player

    playerActions :: [PlayerAction]
    playerActions = L.nub $ L.concat $ do
      player <- S.elems players
      guard (_id player == pid)
      card   <- cards
      peg    <- _pegs player
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
      pg    <- _pegs player
      case pg of
        PegBoard bp Stake -> return $ BP bp
        _                 -> []

    blockedTargetPositions :: S.Set Position
    blockedTargetPositions = S.fromList $ do
      player <- S.elems players
      guard (_id player == pid)
      pg    <- _pegs player
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
  ppeg   <- _pegs player
  guard (onBoard ppeg)
  guard (_id player /= pid && pegMode ppeg == Stake)
  return $ SwitchPegs (_id player) peg ppeg
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
hasAlreadyExchangedCard Player {..} = gets $ M.lookup _id . _cardExchange

canExchangeCardsAmongPlayers :: GameMonad Bool
canExchangeCardsAmongPlayers = do
  GameState       {..} <- get
  GameEnvironment {..} <- ask
  return $ length (M.keys _cardExchange) == _geNPlayers

performCardExchange :: GameMonad ()
performCardExchange = do
  GameState {..} <- get
  tell ["Exchanging cards among players with the following cards: " ++ show _cardExchange]
  -- TODO: Use lenses to clean up
  modify' (\s -> s
            { _players         = exchangeCards' _players _cardExchange
            , _cardExchange    = M.empty
            , _mode            = Play (nextPlayer _roundPlayerTurn)
            , _roundPlayerTurn = nextPlayer _roundPlayerTurn
            })

  where
    exchangeCards' :: S.Set Player -> M.Map PlayerId Card -> S.Set Player
    exchangeCards' players cardsToExchange = S.fromList $ do
      player@Player {..} <- S.elems players
      case M.lookup (partner _id) cardsToExchange of
        Nothing   -> return player
        Just card -> return player { _cards = card : _cards }

giveCard :: Player -> Card -> GameMonad ()
giveCard player card = do

  GameState {..} <- get

  case L.find (== card) (_cards player) of
    Nothing ->
      throwError $ CardNotAvailabe (_uuid player) card

    Just _ ->
      let cardExchange' = M.insert (_id player) card _cardExchange
          hand' = L.delete card (_cards player)
          players' = S.map (\p ->
            if _id p == _id player
            then p { _cards = hand' }
            else p) _players
      in
        -- TODO: use lenses
        modify' (\s -> s
          { _cardExchange = cardExchange'
          , _players = players'
          })

  return ()

showGameState :: GameState -> String
showGameState gameState = unlines
  [ "Mode: " ++ show (_mode gameState)
  , "Deck: " ++ show (_deck gameState)
  , "CardExchange: " ++ show (_cardExchange gameState)
  , "Players: " ++ unlines ["  " ++ showPlayer p | p <- S.elems (_players gameState)]
  ]
  where
    showPlayer :: Player -> String
    showPlayer p = unlines
      [ "PlayerId: " ++ show (_id p)
      , "PlayerUUID: " ++ show (_uuid p)
      , "Hand: " ++ show (_cards p)
      , "Pegs: " ++ show (_pegs p)
      ]

checkPlayerNumber :: Int -> GameMonad ()
checkPlayerNumber n = do
  gameState <- get
  let nPlayers = length (_players gameState)
  when (nPlayers /= n) $ throwError $ WrongNumberPlayers nPlayers

checkPlayerTurn :: Player -> GameMonad Bool
checkPlayerTurn player = do
  gameState <- get
  case _mode gameState of
    Play pid     -> return (pid == _id player)  -- ^ Only the player with the assiged pid can play
    CardExchange -> return True                 -- ^ Anyone can exchange cards at the same time
    _            -> return False                -- ^ Not the player's turn

mkDeck :: GameMonad Deck
mkDeck = do
  deck <- shuffleM defaultDeck
  modify' $ addDeck deck
  return deck

  where
    addDeck :: Deck -> GameState -> GameState
    addDeck deck s = s { _deck = deck }

initGame :: GameMonad ()
initGame = mkDeck >> deal

deal :: GameMonad ()
deal = do
  GameState {..} <- get
  case dealCards _deck of
    Nothing -> do
      tell ["Not enough cards in Deck, making a new Deck"]
      mkDeck
      deal    -- Deal again

    Just (deck', hands) -> do
      tell ["Dealing cards from deck"]
      modify' (\s -> s { _deck    = deck'
                       , _players = giveHand hands _players
                       , _mode    = CardExchange
                       })

  where
    giveHand :: [Hand] -> S.Set Player -> S.Set Player
    giveHand hands players =
      S.fromList [p { _cards = h } | (h, p) <- zip hands (S.elems players)]


join :: GameMonad (PlayerUUID, PlayerId)
join = do
  GameState {..} <- get
  case mkNextPlayerId _players of
    Nothing       -> do
      tell ["Cannot add a new player, the party is full"]
      throwError JoinTooManyPlayers
    Just playerId -> do
      player@Player {..} <- mkPlayer playerId emptyHand defaultPegs
      tell ["Adding new Player to the Game: " ++ show player]
      modify' $ addPlayer player
      return (_uuid, _id)

  where
    -- TODO: use lens to remove the boilerplate
    addPlayer :: Player -> GameState -> GameState
    addPlayer player s = s { _players = S.insert player (_players s) }

readyToPlay :: GameMonad Bool
readyToPlay = do
  GameState       {..} <- get
  GameEnvironment {..} <- ask
  return $ length _players == _geNPlayers

getState :: PlayerUUID -> GameMonad FilteredGameState
getState uuid = do
  gameState <- get
  tell ["[uuid: " ++ show uuid ++ "] Requesting GameState"]
  case findPlayer gameState uuid of
    Nothing     -> throwError $ Unauthorized uuid
    Just player -> return $ filterGameState player gameState

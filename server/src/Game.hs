{-# LANGUAGE ScopedTypeVariables #-}

module Game where

import           Control.Monad         (guard, when)
import           Control.Monad.Except  (throwError)
import           Control.Monad.State   (get, gets, modify')
import           Control.Monad.Writer  (tell)
import           Data.List             as L
import qualified Data.Map              as M
import           Data.Maybe            (maybeToList)
import qualified Data.Set              as S
import           Data.String           (unlines)
import           System.Random.Shuffle (shuffleM)


import           Data.Game
import           GameMonad


play :: Action -> GameMonad ()
play _  = do
    _ <- get
    return ()

-- TODO: finish me to be able to play the TakTic game!
handle :: Action -> GameMonad GameResult
handle (NPA JoinGame) = join

handle (PA player (Exchange card)) = do
  mCardExchanged <- hasAlreadyExchangedCard player
  case mCardExchanged of
    Just c' -> throwError $ CardAlreadyExchanged c' (_uuid player)
    Nothing -> do
      tell ["Giving Card " ++ show card ++ " to Player " ++ show (_id player)]
      giveCard player card
      canExchange <- canExchangeCards
      when canExchange $ do
        gameState <- get
        tell ["Exchanging cards among players with the following cards: " ++ show (_cardExchange gameState)]
        exchangeCards
      return Unit
handle act@(PA player action@(Move card peg position)) = do
  gameState <- get
  let playerActions = cardActions (_players gameState) (_id player)
  if action `L.notElem` playerActions
  then throwError $ InvalidAction act
  else do
    tell ["Performing valid action: " ++ show action]
    -- TODO: implement movePeg
    -- TODO: add some logging
    -- movePeg player peg position >> useCard card >> return Unit
    return Unit

handle (PA p QuitGame)                  = quitGame p
handle _                                = undefined

movePeg :: Player -> Peg -> Position -> GameMonad ()
movePeg player peg position = return ()

-- TODO
useCard :: Player -> Card -> GameMonad ()
useCard player card = return ()

-- TODO: add state for players who left
quitGame :: Player -> GameMonad GameResult
quitGame player = do
  modify' (\s -> s { _mode = Winner winnerTeam })
  return Unit

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
                                     (move pid card peg)

cardActions :: S.Set Player -> PlayerId -> [PlayerAction]
cardActions players pid =
  case playerActions of
    [] -> discards
    _  ->  playerActions

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

move :: PlayerId -> Card -> Peg -> [Position]
move pid card PegHome
  | startCard card = return $ BP (startBoardPosition pid)
  | otherwise      = []
move _ card (PegTarget tp) = TP <$> maybeToList (moveTarget card tp)
move pid card (PegBoard bp _) =
  BP bp' : (TP <$> maybeToList tp')
  where
    bp' :: BoardPosition
    bp' = moveBoard card bp

    tp' :: Maybe TargetPosition
    tp' = moveFromBoardToTarget pid card bp

switch :: S.Set Player -> PlayerId -> Card -> Peg -> [PlayerAction]
switch players pid Switch peg@(PegBoard _ _) = do
  player <- S.elems players
  ppeg <- _pegs player
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
hasAlreadyExchangedCard player = gets $ M.lookup (_id player) . _cardExchange

canExchangeCards :: GameMonad Bool
canExchangeCards = do
  gameState <- get
  return $ length (M.keys (_cardExchange gameState)) == 4

exchangeCards :: GameMonad ()
exchangeCards = do
  gameState <- get
  -- TODO: Use lenses to clean up
  modify' (\s -> s
            { _players = exchangeCards' (_players gameState) (_cardExchange gameState)
            , _cardExchange = M.empty
            , _mode = Play (_roundPlayerTurn s)
            , _roundPlayerTurn = nextPlayer (_roundPlayerTurn s)
            })
  where
    exchangeCards' :: S.Set Player -> M.Map PlayerId Card -> S.Set Player
    exchangeCards' players cardsToExchange = S.fromList $ do
      player <- S.elems players
      case M.lookup (partner (_id player)) cardsToExchange of
        Nothing   -> return player
        Just card -> return player { _cards = card : _cards player }

giveCard :: Player -> Card -> GameMonad GameResult
giveCard player card = do

  gameState <- get

  case L.find (== card) (_cards player) of
    Nothing ->
      throwError $ CardNotAvailabe (_uuid player) card

    Just _ ->
      let cardExchange' = M.insert (_id player) card (_cardExchange gameState)
          hand' = L.delete card (_cards player)
          players' = S.map (\p ->
            if _id p == _id player
            then p { _cards = hand' }
            else p) (_players gameState)
      in
        -- TODO: use lenses
        modify' (\s -> s
          { _cardExchange = cardExchange'
          , _players = players'
          })

  return Unit

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
initGame = do
  checkPlayerNumber 4
  mkDeck
  deal
  return ()

deal :: GameMonad ()
deal = do
  gameState <- get
  case dealCards (_deck gameState) of
    Nothing -> do
      tell ["Not enough cards in Deck, making a new Deck"]
      mkDeck
      deal

    Just (deck', hands) -> do
      tell ["Dealing cards from deck"]
      modify' (\s -> s { _deck = deck'
                       , _players = giveHand hands (_players gameState)
                       , _mode = CardExchange
                       })

  where
    giveHand :: [Hand] -> S.Set Player -> S.Set Player
    giveHand hands players =
      S.fromList [p { _cards = h } | (h, p) <- zip hands (S.elems players)]


join :: GameMonad GameResult
join = do

  gameState <- get

  case mkNextPlayerId (_players gameState) of
    Nothing       -> do
      tell ["Cannot add a new player, the party is full"]
      throwError JoinTooManyPlayers

    Just playerId -> do
      player <- mkPlayer playerId emptyHand defaultPegs
      tell ["Adding new Player to the Game: " ++ show player]
      modify' $ addPlayer player
      when (length (_players gameState) == 3) initGame
      return $ NewPlayer (_uuid player) (_id player)

  where
    -- TODO: use lens to remove the boilerplate
    addPlayer :: Player -> GameState -> GameState
    addPlayer player s = s { _players = S.insert player (_players s) }

getState :: PlayerUUID -> GameMonad FilteredGameState
getState uuid = do
  gameState <- get
  tell ["[uuid: " ++ show uuid ++ "] Requesting GameState"]
  case findPlayer gameState uuid of
    Nothing     -> throwError $ Unauthorized uuid
    Just player -> return $ filterGameState player gameState

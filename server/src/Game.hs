{-# LANGUAGE ScopedTypeVariables #-}

module Game where

import           Control.Monad         (when)
import           Control.Monad.Except  (throwError)
import           Control.Monad.State   (get, gets, modify')
import           Control.Monad.Writer  (tell)
import           Data.Foldable         (for_)
import           Data.List             as L
import qualified Data.Map              as M
import qualified Data.Set              as S
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

handle (PA p (Exchange c)) = do
  mCardExchanged <- hasAlreadyExchangedCard p
  case mCardExchanged of
    Just c' -> throwError $ CardAlreadyExchanged c' (_uuid p)
    Nothing -> do
      tell ["Giving Card " ++ show c ++ " to Player " ++ show (_id p)]
      giveCard p c
      canExchange <- canExchangeCards
      when canExchange $ do
        gameState <- get
        tell ["Exchanging cards among players with the following cards: " ++ show (_cardExchange gameState)]
        exchangeCards
      return Unit
-- handle (PA p (Move c peg position))     = undefined
-- handle (PA p (Discard c))               = undefined
-- handle (PA p (SwitchPegs to peg1 peg2)) = undefined

handle (PA p QuitGame)                  = quitGame p
handle _                                = undefined

-- TODO: add state for players who left
quitGame :: Player -> GameMonad GameResult
quitGame player = do
  modify' (\s -> s { _mode = Winner winnerTeam })
  return Unit

  where
    winnerTeam :: Team
    winnerTeam = nextTeam $ playerTeam $ _id player

generatePossibleActions :: GameState -> Player -> [PlayerAction]
generatePossibleActions gameState player = [QuitGame]

canReach :: GameState -> Peg -> Position -> Bool
canReach gameState peg position = undefined

move :: GameState -> Player -> Peg -> Position -> GameState
move gameState player peg position = undefined

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

printGameState :: GameState -> IO ()
printGameState gameState = do

  putStrLn ""

  -- Game Mode
  putStrLn $ "Mode: " ++ show (_mode gameState)

  -- Deck
  putStrLn $ "Deck: " ++ show (_deck gameState)

  -- CardExchange
  putStrLn $ "CardExchange: " ++ show (_cardExchange gameState)

  -- Player Information
  putStrLn ""
  for_ players $ \p -> do
    putStrLn $ "PlayerId: " ++ show (_id p)
    putStrLn $ "  PlayerUUID: " ++ show (_uuid p)
    putStrLn $ "  Hand: " ++ show (_cards p)
    putStrLn $ "  Pegs: " ++ show (_pegs p)
    putStrLn ""

  where
    players :: [Player]
    players = S.elems $ _players gameState

checkPlayerNumber :: Int -> GameMonad ()
checkPlayerNumber n = do
  gameState <- get
  let nPlayers = length (_players gameState)
  when (nPlayers /= n) $ throwError $ WrongNumberPlayers nPlayers

checkPlayerTurn :: Player -> GameMonad Bool
checkPlayerTurn player = do
  gameState <- get
  case _mode gameState of
    Play pid     -> return (pid == _id player)
    _            -> return False

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

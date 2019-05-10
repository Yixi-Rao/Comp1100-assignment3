module SushiGo where

import Data.List (sort, transpose)
import Data.Ord (comparing)
import ListUtils (counts, pick)
import TextLayout (grid)

data Card
  = Nigiri Int
  | Wasabi (Maybe Card)
  | Dumplings
  | Eel
  | Tofu
  | Sashimi
  | Chopsticks
  deriving (Eq, Ord, Show)

-- | The moves players can make in this game. Note the use of
-- recursion - every Move ends with a TakeCard, but can have zero or
-- more UseChopsticks before that.
--
-- COMP1100 students will never need to UseChopsticks, as their games
-- will not feature that card.
data Move
  = TakeCard Card
  | UseChopsticks Card Move
  deriving (Eq, Show)

data GameStatus
  = Turn Player
  | Finished
  deriving (Eq, Show)

data Player = Player1 | Player2 deriving (Eq, Show)

otherPlayer :: Player -> Player
otherPlayer Player1 = Player2
otherPlayer Player2 = Player1

data GameState = GameState
  GameStatus -- ^ Whose turn it is, or whether the game is over
  [Card] -- ^ Player 1's hand (cards to select from)
  [Card] -- ^ Player 1's chosen cards
  [Card] -- ^ Player 2's hand
  [Card] -- ^ Player 2's cards
  deriving (Eq, Show)

-- | Set up a game
initialGame
  :: [Card] -- ^ Player 1's hand
  -> [Card] -- ^ Player 2's hand
  -> GameState
initialGame p1h p2h = GameState (Turn Player1) p1h [] p2h []

gameStatus :: GameState -> GameStatus
gameStatus (GameState s _ _ _ _) = s

-- | Get the hand for a given player.
handFor :: Player -> GameState -> [Card]
handFor Player1 (GameState _ p1h _ _ _) = p1h
handFor Player2 (GameState _ _ _ p2h _) = p2h

-- | Replace the hand for a given player.
setHandFor :: Player -> [Card] -> GameState -> GameState
setHandFor Player1 p1h (GameState st _ p1c p2h p2c)
  = GameState st p1h p1c p2h p2c
setHandFor Player2 p2h (GameState st p1h p1c _ p2c)
  = GameState st p1h p1c p2h p2c

-- | Get the selected cards for a given player.
cardsFor :: Player -> GameState -> [Card]
cardsFor Player1 (GameState _ _ p1c _ _) = p1c
cardsFor Player2 (GameState _ _ _ _ p2c) = p2c

-- | Replace the selected cards for a given player.
setCardsFor :: Player -> [Card] -> GameState -> GameState
setCardsFor Player1 p1c (GameState st p1h _ p2h p2c)
  = GameState st p1h p1c p2h p2c
setCardsFor Player2 p2c (GameState st p1h p1c p2h _)
  = GameState st p1h p1c p2h p2c

playMove :: Move -> GameState -> GameState
playMove m s = case gameStatus s of
  Finished -> error "playMove: move played against finished game"
  Turn p -> updateStatus (swapHandsMaybe (runMoveFor p s))
    where
      -- Move cards from a player's hand to their cards.
      runMoveFor :: Player -> GameState -> GameState
      runMoveFor Player1 (GameState st p1h p1c p2h p2c)
        = GameState st p1h' p1c' p2h p2c
        where (p1h', p1c') = runMove m p1h p1c

      runMoveFor Player2 (GameState st p1h p1c p2h p2c)
        = GameState st p1h p1c p2h' p2c'
        where (p2h', p2c') = runMove m p2h p2c

      -- Construct the new hand/card pair from the old hand/cards and
      -- move to make.
      runMove :: Move -> [Card] -> [Card] -> ([Card], [Card])
      runMove (TakeCard c) hand cards = pickCard c hand cards
      runMove (UseChopsticks c mv) hand cards =
        case pick Chopsticks cards of
          Nothing -> error "runMove: Using chopsticks without any in hand"
          Just cardsWithoutChopsticks -> runMove mv hand' cards'
            where (hand', cards')
                    = pickCard c (Chopsticks:hand) cardsWithoutChopsticks

      -- If player 2 just had their turn, we need to swap hands for
      -- the next round.
      swapHandsMaybe :: GameState -> GameState
      swapHandsMaybe gs@(GameState st p1h p1c p2h p2c)
        = case p of
            Player1 -> gs
            Player2 -> (GameState st p2h p1c p1h p2c)

      -- Decide who is to go next, or whether the game is over.
      updateStatus :: GameState -> GameState
      updateStatus (GameState _ p1h p1c p2h p2c) = GameState s' p1h p1c p2h p2c
        where s' = case (p1h, p2h) of
                     ([], []) -> Finished
                     _ -> Turn (otherPlayer p)

-- Pick one card from a hand, attaching Nigiri to any free Wasabi.
pickCard :: Card -> [Card] -> [Card] -> ([Card], [Card])
pickCard c hand cards = case pick c hand of
  Nothing -> error (concat
                     [ "pickCard: card "
                     , show c
                     , " not in hand "
                     , show hand
                     ])
  Just hand' -> case (c, pick (Wasabi Nothing) cards) of
    (Nigiri _, Just cards') -> (hand', Wasabi (Just c) : cards')
    _ -> (hand', c:cards)

-- Scores a list of cards
scoreCards :: [Card] -> Int
scoreCards cards = sum
  [ scoreSingleCards
  , scoreDumplings (count Dumplings)
  , scoreEel (count Eel)
  , scoreTofu (count Tofu)
  , scoreSashimi (count Sashimi)
  ]
  where
    scoreSingleCards = sum (flip map cards
      (\card -> case card of
          Nigiri n -> n
          Wasabi (Just (Nigiri n)) -> n * 3
          -- Technically redundant, but we spell it out so every card
          -- type is named in the scoring function.
          Chopsticks -> 0
          _ -> 0))

    scoreDumplings n
      | n == 0 = 0
      | n == 1 = 1
      | n == 2 = 3
      | n == 3 = 6
      | n == 4 = 10
      | n >= 5 = 15
      | otherwise = error "scoreDumplings: negative Dumpling count?"

    scoreEel n
      | n == 0 = 0
      | n == 1 = -3
      | n >= 2 = 7
      | otherwise = error "scoreEel: negative Eel count?"

    scoreTofu n
      | n == 0 = 0
      | n == 1 = 2
      | n == 2 = 6
      | n >= 3 = 0
      | otherwise = error "scoreTofu: negative Tofu count?"

    scoreSashimi n = n `div` 3 * 10

    count card = length (filter (== card) cards)

-- | Who is ahead right now? Return 'Nothing' if scores are tied.
playerAhead :: GameState -> Maybe Player
playerAhead (GameState _ _ p1c _ p2c) = case comparing scoreCards p1c p2c of
  LT -> Just Player2
  EQ -> Nothing
  GT -> Just Player1

-- | Pretty-print a GameState.
renderGame :: GameState -> String
renderGame gs = concat
  [ line
  , header ++ "\n"
  , "\n"
  , "State of Play:\n"
  , "\n"
  , grid (transpose report)
  , "Player 1 Score: " ++ show (scoreCards (cardsFor Player1 gs)) ++ "\n"
  , "Player 2 Score: " ++ show (scoreCards (cardsFor Player2 gs)) ++ "\n"
  , line
  ]
  where
    line = replicate 70 '-' ++ "\n"
    header = case gameStatus gs of
      Turn p -> "Current Player: " ++ show p
      Finished -> "Game Over"

    report :: [[String]]
    report =
      map (take maxLength . (++ repeat "")) [p1Hand, p1Cards, p2Hand, p2Cards]

    p1Hand = "P1 (hand)" : renderCount (counts (handFor Player1 gs))
    p1Cards = "P1 (cards)" : renderCount (counts (cardsFor Player1 gs))
    p2Hand = "P2 (hand)" : renderCount (counts (handFor Player2 gs))
    p2Cards = "P2 (cards)" : renderCount (counts (cardsFor Player2 gs))
    maxLength = maximum (map length [p1Hand, p1Cards, p2Hand, p2Cards])

    renderCount = map renderLine . sort
      where renderLine (c, n) = concat [ "  ", show c, "(", show n, ")" ]

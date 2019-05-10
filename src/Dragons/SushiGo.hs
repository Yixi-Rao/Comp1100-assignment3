module Dragons.SushiGo where

import Control.Monad (replicateM)
import SushiGo
import System.Random (randomRIO)

genInitialHand :: Bool -> IO [Card]
genInitialHand withChopsticks
  | withChopsticks = replicateM handSize (genFromTable chopsticksTable)
  | otherwise = replicateM handSize (genFromTable defaultTable)
  where
    handSize = 15

    chopsticksTable = defaultTable ++ [(3, Chopsticks)]
    defaultTable =
      [ (4, Nigiri 1)
      , (5, Nigiri 2)
      , (3, Nigiri 3)
      , (3, Wasabi Nothing)
      , (8, Dumplings)
      , (8, Eel)
      , (8, Tofu)
      , (8, Sashimi)
      ]

    -- | Select an @a@ from a table of relative frequencies.
    --
    -- NOTE: This is sampling with replacement, which is not how
    -- shuffling cards works.
    genFromTable :: [(Int, a)] -> IO a
    genFromTable table = do
      let limit = sum (map fst table)
          select _ [] = error "table exhausted"
          select x ((freq, a):as)
            | x > freq = select (x - freq) as
            | otherwise = a
      n <- randomRIO (0, limit)
      pure (select n table)

runGame
  :: GameState -- ^ Initial state
  -> (GameState -> IO (Maybe Move)) -- ^ Player 1 move function
  -> (GameState -> IO (Maybe Move)) -- ^ Player 2 move function
  -> IO GameState -- ^ Final state
runGame gs p1Func p2Func =
  case gameStatus gs of
    Turn Player1 -> do
      mMove <- p1Func gs
      case mMove of
        Nothing -> error "player 1 did not make a move"
        Just m -> runGame (playMove m gs) p1Func p2Func

    Turn Player2 -> do
      mMove <- p2Func gs
      case mMove of
        Nothing -> error "player 2 did not make a move"
        Just m -> runGame (playMove m gs) p1Func p2Func

    Finished -> pure gs
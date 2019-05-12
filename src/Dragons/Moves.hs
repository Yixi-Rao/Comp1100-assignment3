{-# LANGUAGE BangPatterns #-}

module Dragons.Moves where

import AI (AIFunc, ais)
import Data.Char (toUpper)
import Data.Foldable (for_)
import Data.Function ((&))
import Data.IORef
import Data.List (nub, sort)
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)
import SushiGo
import System.Random (randomRIO)
import System.Timeout (timeout)

type MoveFunc = GameState -> IO (Maybe Move)
type USecs = Int

lookupAIFunc :: String -> AIFunc
lookupAIFunc name
  = fromMaybe (error ("No AI called " ++ show name)) (lookup name ais)

getMoveAI :: USecs -> AIFunc -> MoveFunc
getMoveAI timeLimit ai game = do
  bestMove <- newIORef Nothing
  let
    loop depth = do
      let
        force m = case m of
          TakeCard !_ -> m
          UseChopsticks !_ m' -> force m' `seq` m
        move = ai game depth
      force move `seq` writeIORef bestMove (Just move)
      loop (depth + 1)

  _ <- timeout timeLimit (loop 1)
  readIORef bestMove

getMoveHuman :: MoveFunc
getMoveHuman gs = do
  let player = case gameStatus gs of
        Turn p -> p
        _ -> error "getMoveHuman: called against finished game"
      hand = handFor player gs
      cards = cardsFor player gs
      handOptions = sort (nub hand)
      hasChopsticks = Chopsticks `elem` cards
      menuOptions = cardOptions ++ chopsticksOptions

      cardOptions = zipWith
        (\label card -> ( label:[]
                        , ("Take " ++ show card, Left card)
                        ))
        ['A'..'Z']
        handOptions
      chopsticksOptions
        | hasChopsticks = zipWith
          (\label card -> ( 'X':label:[]
                          , ("Take " ++ show card ++ " with chopsticks"
                          , Right card)
                          ))
          ['A'..'Z']
          handOptions
        | otherwise = []

  putStrLn (renderGame gs)

  for_ menuOptions
    (\(option, (label, _)) -> putStrLn (concat [ option, ": ", label]))
  putStr "\nEnter choice: "
  choice <- getLine
  case lookup (map toUpper choice) menuOptions of
    Nothing -> do
      putStrLn "Invalid Choice."
      getMoveHuman gs
    Just (_, action) -> case action of
      Left cardToTake -> pure (Just (TakeCard cardToTake))
      Right cardToTakeWithChopsticks -> do
        let (hand', cards')
              = swap
              . uncurry (pickCard Chopsticks) -- return the chopsticks
              . swap
              $ (pickCard cardToTakeWithChopsticks) hand cards -- take card
            gs' = gs
              & setHandFor player hand'
              & setCardsFor player cards'
        m <- getMoveHuman gs'
        putStrLn ""
        pure (fmap (UseChopsticks cardToTakeWithChopsticks) m)

getMoveRandom :: MoveFunc
getMoveRandom gs = do
  let player = case gameStatus gs of
        Turn p -> p
        _ -> error "getMoveRandom: called against finished game"
      hand = handFor player gs
  n <- randomRIO (0, length hand - 1)
  pure (Just (TakeCard (hand !! n)))

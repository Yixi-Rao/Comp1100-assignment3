module Main where

import Control.Monad (replicateM)
import Data.Maybe (fromMaybe)
import Dragons.Moves (MoveFunc, USecs, lookupAIFunc, getMoveAI, getMoveRandom)
import Dragons.SushiGo (genInitialHand, runGame)
import ListUtils (counts)
import SushiGo
import System.Environment (getArgs)
import System.IO (BufferMode(..), hSetBuffering, stdout)

data Args = Args
  Bool -- ^ Add chopsticks cards (COMP1130)
  MoveFunc -- ^ Player 1
  MoveFunc -- ^ Player 2

defaultArgs :: Args
defaultArgs = Args False getMoveRandom getMoveRandom

-- | You may tweak this in your own tests, but we will use 4 seconds
-- in ours.
timeLimit :: USecs
timeLimit = 4 * 1000 * 1000

processArgs :: [String] -> Args
processArgs = go defaultArgs where
  go :: Args -> [String] -> Args
  go args@(Args chopsticks p1func p2func) strings = case strings of
    [] -> args
    "-chopsticks":ss -> go (Args True p1func p2func) ss
    "-p1ai":name:ss ->
      go (Args chopsticks (getMoveAI timeLimit (lookupAIFunc name)) p2func) ss
    "-p1random":ss -> go (Args chopsticks getMoveRandom p2func) ss
    "-p2ai":name:ss ->
      go (Args chopsticks p1func (getMoveAI timeLimit (lookupAIFunc name))) ss
    "-p2random":ss -> go (Args chopsticks p1func getMoveRandom) ss
    _ -> error ("Cannot parse arguments: " ++ show strings)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  Args chopsticks p1func p2func <- fmap processArgs getArgs
  results <- replicateM 10 (oneGame chopsticks p1func p2func)
  putStrLn ""

  let
    countedResults = counts results
    p1Wins = fromMaybe 0 (lookup (Just Player1) countedResults)
    p2Wins = fromMaybe 0 (lookup (Just Player2) countedResults)
    ties = fromMaybe 0 (lookup Nothing countedResults)

  putStrLn ("Player 1 Wins: " ++ show p1Wins)
  putStrLn ("Player 2 Wins: " ++ show p2Wins)
  putStrLn ("Ties: " ++ show ties)

oneGame :: Bool -> MoveFunc -> MoveFunc -> IO (Maybe Player)
oneGame chopsticks p1Func p2Func = do
  p1h <- genInitialHand chopsticks
  p2h <- genInitialHand chopsticks

  let initialState = initialGame p1h p2h
  finalState <- runGame initialState p1Func p2Func
  let winner = playerAhead finalState

  putStr $ case winner of
    Nothing -> "T"
    Just Player1 -> "1"
    Just Player2 -> "2"

  pure winner

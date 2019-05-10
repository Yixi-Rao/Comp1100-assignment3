module Main where

import AI (AIFunc)
import Dragons.Moves
  ( MoveFunc
  , USecs
  , lookupAIFunc
  , getMoveAI
  , getMoveHuman
  , getMoveRandom
  )
import Dragons.SushiGo (genInitialHand, runGame)
import SushiGo
import System.Environment (getArgs)
import System.IO (BufferMode(..), hSetBuffering, stdout)

data PlayerSetting = AI AIFunc | Human | Random

data Args = Args
  Bool -- ^ Add chopsticks cards (COMP1130)
  PlayerSetting -- ^ Player 1
  PlayerSetting -- ^ Player 2

defaultArgs :: Args
defaultArgs = (Args False Human (AI (lookupAIFunc "default")))

-- | You may tweak this in your own tests, but we will use 4 seconds
-- in ours.
timeLimit :: USecs
timeLimit = 4 * 1000 * 1000

processArgs :: [String] -> Args
processArgs = go defaultArgs where
  go :: Args -> [String] -> Args
  go args@(Args chopsticks p1 p2) strings = case strings of
    [] -> args
    "-chopsticks":ss -> go (Args True p1 p2) ss
    "-p1ai":name:ss -> go (Args chopsticks (AI (lookupAIFunc name)) p2) ss
    "-p1human":ss -> go (Args chopsticks Human p2) ss
    "-p1random":ss -> go (Args chopsticks Random p2) ss
    "-p2ai":name:ss -> go (Args chopsticks p1 (AI (lookupAIFunc name))) ss
    "-p2human":ss -> go (Args chopsticks p1 Human) ss
    "-p2random":ss -> go (Args chopsticks p1 Random) ss
    _ -> error ("Cannot parse arguments: " ++ show strings)

getMoveFunc :: PlayerSetting -> MoveFunc
getMoveFunc (AI a) = getMoveAI timeLimit a
getMoveFunc Human = getMoveHuman
getMoveFunc Random = getMoveRandom

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  (Args withChopsticks p1 p2) <- fmap processArgs getArgs
  p1h <- genInitialHand withChopsticks
  p2h <- genInitialHand withChopsticks

  let p1Func = getMoveFunc p1
      p2Func = getMoveFunc p2
      initialState = initialGame p1h p2h

  finalState <- runGame initialState p1Func p2Func

  putStrLn "******************** Final State ********************\n"
  putStrLn (renderGame finalState)
  case playerAhead finalState of
    Just Player1 -> putStrLn "Player 1 Wins!"
    Just Player2 -> putStrLn "Player 2 Wins!"
    Nothing -> putStrLn "Tie!"

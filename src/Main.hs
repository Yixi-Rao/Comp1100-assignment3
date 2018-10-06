{-# LANGUAGE Unsafe #-}
{-|
Module      : Main
Description : Main entry point to the Othello game
Copyright   : (c) Robert 'Probie' Offner, 2018
License     : GPL-3
-}
module Main where

import safe AI
import Config
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Char (ord, toLower)
import Dragons.Timeout
import Dragons.Config
import Dragons.GUI
import Game
import GameState

main :: IO ()
main = do
  config <- parseGameConfig
  case config of
    HelpConfig message -> putStrLn message
    ConsoleConfig p1 p2 timeout ->
      playConsoleGame p1 p2 timeout initialGame
    GUIConfig p1 p2 timeout ->
      playGUIGame p1 p2 timeout initialGame
    NetHostConfig{} -> error "Hosting network game not yet implemented"
    NetClientConfig{} -> error "Joining network game not yet implemented"
    TournamentConfig{} -> error "Tournament game not yet implemented"

playConsoleGame :: Maybe AI -> Maybe AI -> Double -> Game -> IO ()
playConsoleGame player1 player2 timeout = go 
  where
    -- Step the game
    go game = do
      putStrLn (ppGame game)
      case turn game of
        Nothing -> return ()
        Just Dark -> darkMove game >>= go
        Just Light -> lightMove game >>= go
    -- Move for the dark player
    darkMove = case player1 of
      Nothing -> humanMove Dark
      Just ai -> aiMove ai Dark
    -- Move for the light player
    lightMove = case player2 of
      Nothing -> humanMove Light
      Just ai -> aiMove ai Light
    -- Make a move for an AI
    aiMove ai player game = do
      move <- timeoutTake timeout (zip [1 :: Int ..] (ai timeout game))
      case move of
        Nothing -> error (show player ++ " failed to make a move in time")
        Just (lookahead, (col, row)) ->
          case play col row game of
            Nothing -> error (show player ++ " made an invalid move")
            Just game' -> do
              putStrLn (show player ++ " had a lookahead of " 
                       ++ show lookahead)
              return game'
    -- Make a move for a Human (or more accurately, wait for a move
    -- from a human)
    humanMove player game = do
      putStrLn ("Please make a move (e.g. d3) for " ++ show player)
      move <- getLine
      case map toLower move of
        [c,r] | c `elem` ['a'..'h'],
                r `elem` ['1'..'8'],
                col <- ord c - ord 'a',
                row <- ord r - ord '1',
                Just game' <- play col row game -> return game'
        _ -> do
          putStrLn (ppGame game)
          -- Print "Invalid move" after reprinting the board to
          -- make it clearer something has gone wrong
          putStrLn ("Invalid move (" ++ move ++ ")")
          humanMove player game

playGUIGame :: Maybe AI -> Maybe AI -> Double -> Game -> IO ()
playGUIGame player1 player2 timeout g = do
  putStrLn "See the game at http://127.0.0.1:3000"
  runGUI $ \moves toDraw ->
    -- Visually crash
    (`catch` (\e -> do atomically $ mapM_ (writeTChan toDraw)
                                          [DrawTable, Fail (show e)]
                       throwIO (e :: SomeException))) $ do
      atomically $ mapM_ (writeTChan toDraw) (drawGame g)
      go moves toDraw g
  where
    -- Step the game
    go moves toDraw game@(Game _ board) = do
      atomically $ mapM_ (writeTChan toDraw)
        [ DrawTurn (turn game)
        , DrawScores (currentScore Dark board) (currentScore Light board)]
      case turn game of
        Nothing -> return ()
        Just Dark -> darkMove >>= go moves toDraw
        Just Light -> lightMove >>= go moves toDraw
    -- Move for the dark player
      where
        darkMove = case player1 of
          Nothing -> humanMove Dark
          Just ai -> aiMove ai Dark
        -- Move for the light player
        lightMove = case player2 of
          Nothing -> humanMove Light
          Just ai -> aiMove ai Light
        -- Make a move for an AI
        aiMove ai player = do
          move <- timeoutTake timeout (ai timeout game)
          case move of
            Nothing -> error (show player ++ " failed to make a move in time")
            Just (col, row) ->
              case play col row game of
                Nothing -> error (show player ++ " made an invalid move")
                Just game' -> do
                  animateMove player col row game game' toDraw
                  atomically $ writeTChan toDraw (DrawPiece player col row)
                  return game'
        -- Make a move for a Human (or more accurately, wait for a move
        -- from a human)
        humanMove player = do
          atomically $ flushChan moves -- Clear any moves in the queue
          (col, row) <- atomically $ readTChan moves
          case play col row game of
            Nothing -> do
              atomically $ mapM_ (writeTChan toDraw) [Save, BadMove col row]
              threadDelay 500000
              atomically $ writeTChan toDraw Restore
              humanMove player
            Just game' -> do
              animateMove player col row game game' toDraw
              return game'


animateMove :: Player -> Int -> Int -> Game -> Game -> TChan GUIAction -> IO ()
animateMove player col row (Game _ board) (Game _ board') toDraw = do
  atomically $ writeTChan toDraw (DrawPiece player col row)
  let diffs = [(y,x) | (x, ps) <- zip [0 ..]
                                   (zipWith zip board board')
                     , (y, (p1,p2)) <- zip [0..] ps, p1 /= p2
                     , (y,x) /= (col,row)]
  forM_ diffs $ \(c,r) -> do
    threadDelay 200000
    atomically $ writeTChan toDraw (DrawPiece player c r)


flushChan :: TChan a -> STM ()
flushChan chan = do
  next <- tryReadTChan chan
  case next of
    Nothing -> return ()
    Just _ -> flushChan chan

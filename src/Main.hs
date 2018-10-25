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
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad hiding (join)
import Data.Char (ord, toLower)
import Dragons.Timeout
import Dragons.Config
import Dragons.GUI
import Dragons.Network
import Game
import GameState

-- A transcript for a game - recorded backwards
-- i.e last played move at the head of the list
type Transcript = [(Int,Int)]

-- Convert a transcript to a string in the expected
-- order
ppTranscript :: Transcript -> String
ppTranscript = concatMap (\(col,row) -> [ ['A'..'H'] !! col
                                        , ['1'..'8'] !! row])
             . reverse

main :: IO ()
main = do
  config <- parseGameConfig
  case config of
    HelpConfig message -> putStrLn message
    ConsoleConfig p1 p2 timeout ->
      playConsoleGame p1 p2 timeout initialGame []
    GUIConfig p1 p2 timeout ->
      playGUIGame p1 p2 timeout initialGame []
    NetHostConfig ai timeout port ->
      hostNetGame ai timeout port
    NetClientConfig ai hostname port ->
      joinNetGame ai hostname port
    TournamentConfig{} -> error "Tournament game not yet implemented"

playConsoleGame :: Maybe AI -> Maybe AI -> Double -> Game
                -> Transcript -> IO ()
playConsoleGame player1 player2 timeout = go 
  where
    -- Step the game
    go game transcript = do
      putStrLn (ppGame game)
      case turn game of
        Nothing -> putStrLn $ "Transcript: " ++ ppTranscript transcript
        Just Dark -> darkMove game transcript >>= uncurry go
        Just Light -> lightMove game transcript >>= uncurry go
    -- Move for the dark player
    darkMove = case player1 of
      Nothing -> humanMove Dark
      Just ai -> aiMove ai Dark
    -- Move for the light player
    lightMove = case player2 of
      Nothing -> humanMove Light
      Just ai -> aiMove ai Light
    -- Make a move for an AI
    aiMove ai player game transcript = do
      move <- timeoutTake timeout (zip [1 :: Int ..] (ai timeout game))
      case move of
        Nothing -> error (show player ++ " failed to make a move in time")
        Just (lookahead, (col, row)) ->
          case play col row game of
            Nothing -> error (show player ++ " made an invalid move")
            Just game' -> do
              putStrLn (show player ++ " had a lookahead of " 
                       ++ show lookahead)
              return (game', (col,row):transcript)
    -- Make a move for a Human (or more accurately, wait for a move
    -- from a human)
    humanMove player game transcript = do
      putStrLn ("Please make a move (e.g. d3) for " ++ show player)
      move <- getLine
      case map toLower move of
        [c,r] | c `elem` ['a'..'h'],
                r `elem` ['1'..'8'],
                col <- ord c - ord 'a',
                row <- ord r - ord '1',
                Just game' <- play col row game ->
                  return (game', (col,row):transcript)
        _ -> do
          putStrLn (ppGame game)
          -- Print "Invalid move" after reprinting the board to
          -- make it clearer something has gone wrong
          putStrLn ("Invalid move (" ++ move ++ ")")
          humanMove player game transcript

playGUIGame :: Maybe AI -> Maybe AI -> Double -> Game -> Transcript -> IO ()
playGUIGame player1 player2 timeout g t = do
  putStrLn "See the game at http://127.0.0.1:3000"
  runGUI $ \moves toDraw ->
    -- Visually crash
    (`catch` (\e -> do atomically $ mapM_ (writeTChan toDraw)
                                          [DrawTable, Fail (show e)]
                       throwIO (e :: SomeException))) $ do
      atomically $ mapM_ (writeTChan toDraw) (drawGame g)
      go moves toDraw g t
  where
    -- Step the game
    go moves toDraw game@(Game _ board) transcript = do
      atomically $ mapM_ (writeTChan toDraw)
        [ DrawTurn (turn game)
        , DrawScores (currentScore Dark board) (currentScore Light board)]
      case turn game of
        Nothing -> putStrLn $ "Transcript: " ++ ppTranscript transcript
        Just Dark -> darkMove >>= uncurry (go moves toDraw)
        Just Light -> lightMove >>= uncurry (go moves toDraw)
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
                  return (game', (col,row):transcript)
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
              return (game', (col,row):transcript)


animateMove :: Player -> Int -> Int -> Game -> Game
            -> TChan GUIAction -> IO ()
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

hostNetGame :: AI -> Double -> Integer -> IO ()
hostNetGame ai timeout port = host port (start initialGame [])
  where
    start game transcript recv send end = do
      message <- atomically $ readTChan recv
      case message of
        JoinGame -> do
          atomically $ writeTChan send (StartGame timeout)
          ourMove ai game transcript timeout recv send end
        _ -> error $ "hostNetGame:start: Bad message " ++ show message

joinNetGame :: AI -> String -> Integer -> IO ()
joinNetGame ai hostname port = join hostname port start
  where
    start recv send end = do
      atomically (writeTChan send JoinGame)
      message <- atomically $ readTChan recv
      case message of
        StartGame timeout -> theirMove ai initialGame [] timeout recv send end
        _ -> do
          putStrLn $ "joinNetGame:start : Bad message " ++ show message
          end

endGame :: Game -> Transcript -> TChan NetMessage
        -> TChan NetMessage -> IO () -> IO ()
endGame game transcript recv send end = do
  putStrLn (ppGame game)
  atomically $ writeTChan send Finished
  Just Finished <- readTChanTimeout 1 recv
  putStrLn $ "Transcript: " ++ ppTranscript transcript
  end

ourMove :: AI -> Game -> Transcript -> Double
        -> TChan NetMessage -> TChan NetMessage -> IO () -> IO ()
ourMove _ g@(Game Nothing _) transcript _ recv send end =
  endGame g transcript recv send end
ourMove ai game transcript timeout recv send end = do
  putStrLn (ppGame game)
  move <- timeoutTake timeout (zip [1 :: Int ..] (ai timeout game))
  case move of
    Nothing -> putStrLn "We failed to make a move in time" >> end
    Just (lookahead, (col, row)) ->
      case play col row game of
        Nothing -> putStrLn "We made an invalid move" >> end
        Just game' -> do
          putStrLn ("We  had a lookahead of "
                    ++ show lookahead)
          atomically $ writeTChan send (MakeMove (NetGame game') (col, row))
          (if turn game == turn game' then ourMove else theirMove) ai
            game' ((col,row): transcript) timeout recv send end


theirMove :: AI -> Game -> Transcript -> Double
          -> TChan NetMessage -> TChan NetMessage -> IO () -> IO ()
theirMove _ g@(Game Nothing _) transcript _ recv send end =
  endGame g transcript recv send end
theirMove ai game transcript timeout recv send end = do
  putStrLn (ppGame game)
  move <- readTChanTimeout (timeout+0.2) recv
  case move of
    Nothing -> do
      putStrLn "They failed to make a move in time"
      atomically $ writeTChan send (Disconnect "You took too long")
      end
    Just (MakeMove (NetGame game') (col,row))
      | Just game' /= play col row game -> do
          putStrLn "They made an illegal move"
          atomically $ writeTChan send (Disconnect "They made an illegal move")
          end
      | turn game == turn game' -> theirMove ai game' ((col,row):transcript)
                                     timeout recv send end
      | otherwise -> ourMove ai game' ((col,row):transcript)
                       timeout recv send end
    Just mess -> putStrLn ("Unexpected message: " ++ show mess) >> end



  return ()

readTChanTimeout :: Double -> TChan a -> IO (Maybe a)
readTChanTimeout timeout chan = do
  delay <- registerDelay (round $ timeout * 1000000)
  atomically $
        Just <$> readTChan chan
    <|> Nothing <$ (check <=< readTVar) delay
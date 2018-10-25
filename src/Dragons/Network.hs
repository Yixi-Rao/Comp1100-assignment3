{-# LANGUAGE DeriveGeneric #-}
{-|
Module      : Dragons.Network
Description : Network games
Copyright   : (c) Robert 'Probie' Offner, 2018
License     : GPL-3

Play Othello over the network
-}
module Dragons.Network (host, join, NetGame (..), NetMessage (..)) where

import Control.Exception (finally)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (void, forever)
import Data.Bits
import Data.Binary
import GameState
import Game
import GHC.Generics
import Network.Socket hiding (recv)
import Network.Socket.ByteString.Lazy
import qualified Data.ByteString.Lazy as BS
import System.Timeout (timeout)

-- We need to newtype to avoid orphan instances
newtype NetGame = NetGame Game
  deriving (Eq, Show)

instance Binary NetGame where
  get = do
    p <- get
    darkBoard <- get
    lightBoard <- get
    return $ NetGame
           $ Game (toTurn p)
               (buildBoard (fromBits darkBoard) (fromBits lightBoard))
    where
      toTurn :: Word8 -> Maybe Player
      toTurn 1 = Just Dark
      toTurn 2 = Just Light
      toTurn _ = Nothing
  put (NetGame (Game p board)) = do
    put playerWord
    put darkBoard
    put lightBoard
    where
      playerWord :: Word8
      playerWord = case p of
        Nothing -> 0
        Just Dark -> 1
        Just Light -> 2
      darkBoard :: Word64
      darkBoard = toBits $ map (== Just Dark) $ concat board
      lightBoard :: Word64
      lightBoard = toBits $ map (== Just Light) $ concat board

toBits :: [Bool] -> Word64
toBits = go 0
  where
    go acc [] = acc
    go acc (False:xs) = go (acc `shift` 1) xs
    go acc (True:xs) = go ((acc `shift` 1) + 1) xs

fromBits :: Word64 -> [Bool]
fromBits n = map (testBit n) [63,62..0]

buildBoard :: [Bool] -> [Bool] -> Board
buildBoard darks lights = reshape $ zipWith makePiece darks lights
  where
    makePiece True _ = Just Dark
    makePiece _ True = Just Light
    makePiece _ _    = Nothing
    reshape [] = []
    reshape xs = let (ys,zs) = splitAt 8 xs in ys : reshape zs

data NetMessage
  = JoinGame
  | StartGame Double
  | NextGame
  | MakeMove NetGame (Int, Int)
  | Disconnect String
  | Finished
  deriving (Eq, Show, Generic)

instance Binary NetMessage

sendMessage :: Socket -> NetMessage -> IO ()
sendMessage s = sendAll s . encode

receiveMessages :: Socket -> IO [NetMessage]
receiveMessages s = do
  rawBytes <- grabBytes
  process rawBytes
  where
    grabBytes = do
      bytes <- recv s 1024
      if BS.length bytes == 1024 then do
          next <- timeout 10000 grabBytes -- 10 milliseconds to check for more
          case next of
            Nothing -> return BS.empty
            Just bs -> BS.append bs <$> grabBytes
        else return bytes
    process bytes =
      if BS.length bytes == 0 then return []
        else case decodeOrFail bytes of
               Left (_, _, err) -> error err
               Right (rest, _, x) -> (x:) <$> process rest

-- Host a connection
host :: Integer -> (TChan NetMessage -> TChan NetMessage -> IO () -> IO ()) -> IO ()
host portNum go = do
  let hints = defaultHints { addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV, AI_PASSIVE]
                           , addrSocketType = Stream }
  addr:_ <- getAddrInfo (Just hints) Nothing (Just (show portNum))
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  bind sock (addrAddress addr)
  listen sock 1
  (conn, _) <- accept sock
  finally (sendToChannels go conn) (close sock)

-- Join a connection
join :: String -> Integer -> (TChan NetMessage -> TChan NetMessage
     -> IO () -> IO ()) -> IO ()
join hostname portNum go = do
  let hints = defaultHints { addrSocketType = Stream }
  addr:_ <- getAddrInfo (Just hints) (Just hostname) (Just (show portNum))
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  connect sock $ addrAddress addr
  sendToChannels go sock

-- Pipe network messages via TChans
sendToChannels :: (TChan NetMessage -> TChan NetMessage -> IO () -> IO ())
               -> Socket -> IO ()
sendToChannels go sock = do
  outChan <- atomically newTChan
  inChan <- atomically newTChan
  let sendLoop = forever $ do
        nextMessage <- atomically $ readTChan outChan
        sendMessage sock nextMessage
  let receiveLoop = forever $ do
        nextMessages <- receiveMessages sock
        atomically $ mapM_ (writeTChan inChan) nextMessages
  outThread <- forkIO sendLoop
  thisThread <- myThreadId
  void $ forkIO $ go inChan outChan
    (threadDelay 100000 >> killThread outThread >> killThread thisThread)
  finally receiveLoop (close sock)

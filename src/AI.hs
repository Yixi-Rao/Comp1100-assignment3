{-|
Module      : AI
Description : The AI for Othello
Copyright   : (c) Robert 'Probie' Offner, <your name here>, 2018
License     : GPL-3
-}
module AI where

import Game

-- |  An AI is a function from the time given and the current state of
-- the game to a (potentially infinite) list of increasingly better
-- moves.
type AI = Double -> Game -> [(Int, Int)]

-- | A list of known AIs and their names.
ais :: [(String, AI)]
ais = [("default", makeBestMove)]

-- | The default AI. It just repeatedly applies `makeAMove` to
-- increasingly higher depths.
makeBestMove :: AI
makeBestMove _timeout game = map (makeAMove game) [1..]

-- | Given a `Game` and a lookahead, return the best move to play
makeAMove :: Game -> Int -> (Int, Int)
makeAMove = error "makeAMove: Unimplemented"

{-|
Module      : <AI>
Description : <An Ai to play the Sushigo Game>
Maintainer  : <u6826541@anu.edu.an>
Name        : <Yixi Rao>
Assignment  : <3>

<Ai will decide which move to make to get the highest points>

-}
module AI where

import SushiGo



-- The test program will repeatedly call an AI function with
-- increasing lookahead values until it takes too long to generate a
-- result, and use the final result it returns as the "best" move your
-- AI could find.
type AIFunc
    = GameState -- ^ The current game
    -> Int -- ^ How far you should look ahead
    -> Move

-- | The table of all AIs you have implemented. We will mark the AI
-- called "default" as your submission, but you may include other AIs
-- for testing.
ais :: [(String, AIFunc)]
ais = [("default", bestNextMove),("stupid",firstCard)]

-- Equivalently: firstLegal :: GameState -> Int -> Move
-- firstLegal simply takes the first card it sees
firstCard :: AIFunc
firstCard state _ = case gameStatus state of
    Turn player -> TakeCard (head (handFor player state))
    _ -> error "firstCard: called on finished game"

-- | a Rose tree can enumerate all the possible states of a Game
data Rose a = RoseNode a [Rose a]
    deriving (Eq, Show)

-- Part 1: generate all possible states

-- | Pick an initial game state and generate all the possible next States of a player
pickSushi2 :: GameState -> [GameState]
pickSushi2 gs@(GameState _ p1h p1c p2h p2c) = case gs of
    (GameState _ [] p1c1 [] p2c1) -> [(GameState Finished [] p1c1 [] p2c1)]
    (GameState Finished _ _ _ _) -> error "game is finished, next moves is not allowed"
    (GameState (Turn player) p1h2 _ p2h2 _)
        | player == Player1 -> player1Turn p1h2
        | otherwise ->  player2Turn p2h2

          -- | If it is player 1,then it will return all possible states of what player 1 have chosen
    where player1Turn :: [Card] -> [GameState]
          player1Turn hands = case hands of
              x:xs -> GameState (Turn Player2) (handCanUse handsCards) (cardsChosen handsCards) p2h p2c : player1Turn xs
                  where handsCards = pickCard x p1h p1c
              [] -> []

          -- | If it is player 2,then it will return all possible states of what player 2 have chosen
          player2Turn :: [Card] -> [GameState]
          player2Turn hs = case hs of
            y:ys -> GameState (Turn Player1) (handCanUse handsCards2) p1c p1h (cardsChosen handsCards2) : player2Turn ys
                where handsCards2 = pickCard y p2h p2c
            [] -> []

-- | it will return the hands of pickCard function
handCanUse :: ([Card], [Card]) -> [Card]
handCanUse (a,_) = a

-- | it will return the cards of pickCard function
cardsChosen :: ([Card], [Card]) -> [Card]
cardsChosen (_,b) = b

-- Part 2: Generating the trees

-- | It will create a rose tree, which nodes is GameState
sushiTree :: GameState -> Rose GameState
sushiTree state = RoseNode state (map sushiTree (pickSushi2 state))

-- | It takes an function and maps it to all the nodes of the tree
roseMap :: (a -> b) -> Rose a -> Rose b
roseMap f tree = case tree of
    RoseNode a [] -> RoseNode (f a) []
    RoseNode a list -> RoseNode (f a) (foldr (\x y -> roseMap f x : y) [] list)

-- | It will transform the sushi tree to the Rose tree of score
scoreTree :: GameState -> Rose Int
scoreTree state = roseMap (won) (sushiTree state)

-- | To value all the game state of the tree, if Player 1 scores more, then its score is 1, else -1
won ::  GameState -> Int
won gs
    | scoreCards (cardsFor Player1 gs) > scoreCards (cardsFor Player2 gs) = 1
    | scoreCards (cardsFor Player1 gs) == scoreCards (cardsFor Player2 gs) = 0
    | otherwise = -1

-- Part 3: Alpha-Beta pruning algorithm

-- | Marker will be used as an index of all scores in the tree of some rows
type Marker = Int

-- | Take the maximum of a list of minimas of further value
maximize :: Rose Int -> (Int,Marker)
maximize tree = (maximum (removeMark (maximize' tree)),maximum (getMark (maximize' tree)))

-- | It will remove the markers,and return the scores

removeMark :: [(Int,Int)] -> [Int]
removeMark mix = case mix of
    (a,_):xs -> a : removeMark xs
    [] -> []

-- | Ignore the scores and return all the markers in a list
getMark :: [(Int,Int)] -> [Int]
getMark mix = case mix of
    (_,b):ys -> b: getMark ys
    [] -> []

-- 返回的是，第一个max下面的分支的已经计算好的最小值
maximize' :: Rose Int -> [(Int,Marker)]
maximize' tree = case tree of
    RoseNode a [] -> [(a,0)]
    RoseNode _ list -> mapMin (map minimize' list)

          --
    where mapMin :: [[(Int,Marker)]] -> [(Int,Marker)]
          mapMin (z:zs) = (mayLarge,0): (omit 1 mayLarge zs)
            where mayLarge = (minimum (removeMark z))
          mapMin [] = []



          --
          omit :: Int -> Int -> [[(Int,Marker)]] -> [(Int,Marker)]
          omit marker pot list1 = case list1 of
              [] -> []
              y:ys
                  | minlep pot potRest -> omit (marker+1) pot ys
                  | otherwise -> (minimum potRest,marker) : (omit (marker+1) (minimum potRest) ys)
                    where potRest = removeMark y

          --
          minlep :: Int -> [Int] -> Bool
          minlep pot1 list2 = case list2 of
              [] -> False
              n: ns
                  | pot1 >= n -> True
                  | otherwise -> minlep pot1 ns

minimize' :: Rose Int -> [(Int,Marker)]
minimize' tree = case tree of
    RoseNode a [] -> [(a,0)]
    RoseNode _ list -> mapMax (map maximize' list)

    where mapMax :: [[(Int,Marker)]] -> [(Int,Marker)]
          mapMax (z:zs) = (maySmall,0): (omit 0 maySmall zs)
            where maySmall = maximum (removeMark z)
          mapMax [] = []

          omit :: Int -> Int -> [[(Int,Marker)]] -> [(Int,Marker)]
          omit marker pot list1 = case list1 of
              [] -> []
              y:ys
                  | maxlep pot potRest2 -> omit (marker+1) pot ys
                  | otherwise -> (maximum potRest2,marker+1) : (omit (marker+1) (maximum potRest2) ys)
                    where potRest2 = removeMark y

          maxlep ::  Int->  [Int]-> Bool
          maxlep pot1 list2 = case list2 of
              [] -> False
              n: ns
                  | pot1 <= n -> True
                  | otherwise -> maxlep pot1 ns
-------------------------------------------------------------------------------------------------------------------------------
{-
bestMove :: GameState -> Int
bestMove gs = maximize (prune 5 (scoreTree gs))

prune :: Int -> Rose a -> Rose a
prune n (RoseNode a list)
    |  n == 0 = RoseNode a []
    | otherwise = RoseNode a (map (prune (n-1)) list)

getBestCard :: Int -> [GameState] -> Card
getBestCard best states = case states of
    x:xs
        | scoreCards (cardsFor Player1 x) == best -> wasabiJudge (head (cardsFor Player1 x))
        | otherwise -> getBestCard best xs
    [] -> error "Some thing wrong, card should be in it"
    where wasabiJudge :: Card -> Card
          wasabiJudge card = case card of
            Wasabi _ -> Wasabi Nothing
            _ -> card

bestNextMove ::AIFunc
bestNextMove state n = case gameStatus state of
    Turn _ -> TakeCard (getBestCard (bestMove state) (pickSushi2 state))
    _ -> error "firstCard: called on finished game"
-}
bestMove :: GameState -> (Int,Marker)
bestMove gs = maximize (prune 5 (scoreTree gs))


prune :: Int -> Rose a -> Rose a
prune n (RoseNode a list)
    |  n == 0 = RoseNode a []
    | otherwise = RoseNode a (map (prune (n-1)) list)

getBestCard :: (Int,Marker) -> GameState -> Card
getBestCard (_,marks) state@(GameState (Turn player) _ _ _ _) = wasabiJudge (head (cardsFor player (pickSushi2 state!! marks)))
    where wasabiJudge :: Card -> Card
          wasabiJudge card = case card of
            Wasabi (Just (Nigiri int)) -> Nigiri int
            _ -> card
getBestCard _ (GameState Finished _ _ _ _) = error "it is finished,can not get best card"



bestNextMove ::AIFunc
bestNextMove state _ = case gameStatus state of
    Turn _ -> TakeCard (getBestCard (bestMove state) state)
    _ -> error "firstCard: called on finished game"



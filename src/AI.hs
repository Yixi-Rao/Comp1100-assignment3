{-|
Module      : <AI>
Description : <An Ai to play the Sushigo Game>
Maintainer  : <u6826541@anu.edu.an>
Name        : <Yixi Rao>
Assignment  : <3>

<The test program will repeatedly call an AI function with,increasing lookahead values until it takes too long to generate a
result, and use the final result it returns as the "best" move your AI could find.>

-}
module AI where

import SushiGo



-- The test program will repeatedly call an AI function with
-- increasing lookahead values until it takes too long to generate a
-- result, and use the final result it returns as the "best" move my
-- AI could find.
type AIFunc
    = GameState -- ^ The current game
    -> Int -- ^ How far you should look ahead
    -> Move

-- | The table of all AIs you have implemented.
ais :: [(String, AIFunc)]
ais = [("default", bestNextMove),("stupid",firstCard)]


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
pickSushi :: GameState -> [GameState]
pickSushi gs@(GameState _ p1h p1c p2h p2c) = case gs of
    (GameState _ [] p1c1 [] p2c1) -> [(GameState Finished [] p1c1 [] p2c1)]
    (GameState Finished _ _ _ _) -> error "game is finished, next moves is not allowed"
    (GameState (Turn player) p1h2 _ p2h2 _)
        | player == Player1 -> player1Turn p1h2
        | otherwise ->  player2Turn p2h2

          -- | If it is player 1,then it will return all possible states of what player 1 have chosen
    where player1Turn :: [Card] -> [GameState]
          player1Turn hands = case hands of
              Chopsticks:_ -> error "Chopsticks is not contained in this game"
              [] -> []
              x:xs -> GameState (Turn Player2) (handCanUse handsCards) (cardsChosen handsCards) p2h p2c : player1Turn xs
                  where handsCards = pickCard x p1h p1c

          -- | If it is player 2,then it will return all possible states of what player 2 have chosen
          player2Turn :: [Card] -> [GameState]
          player2Turn hs = case hs of
              Chopsticks:_ -> error "Chopsticks is not contained in this game"
              [] -> []
              y:ys -> GameState (Turn Player1) (handCanUse handsCards2) p1c p1h (cardsChosen handsCards2) : player2Turn ys
                  where handsCards2 = pickCard y p2h p2c

-- | it will return the hands of pickCard function
handCanUse :: ([Card], [Card]) -> [Card]
handCanUse (a,_) = a

-- | it will return the cards of pickCard function
cardsChosen :: ([Card], [Card]) -> [Card]
cardsChosen (_,b) = b

-- Part 2: Generating the trees

-- | It will create a rose tree, which nodes is GameState
sushiTree :: GameState -> Rose GameState
sushiTree state = RoseNode state (map sushiTree (pickSushi state))

-- | It takes an function and maps it to all the nodes of the tree
roseMap :: (a -> b) -> Rose a -> Rose b
roseMap f tree = case tree of
    RoseNode a [] -> RoseNode (f a) []
    RoseNode a list -> RoseNode (f a) (foldr (\x y -> roseMap f x : y) [] list)

-- | It will transform the sushi tree to the Rose tree of score
scoreTree :: GameState -> Rose Int
scoreTree state@(GameState (Turn player) _ _ _ _) = roseMap (won player) (sushiTree state)
scoreTree (GameState Finished _ _ _ _) = error "No more score when game is finished"

-- | To value all the game state of the tree, if Player 1 scores more, then its score is 1, else -1
won ::  Player -> GameState -> Int
won player gs
    | scoreCards (cardsFor player gs) > scoreCards (cardsFor (otherPlayer player) gs) = 1
    | scoreCards (cardsFor player gs) == scoreCards (cardsFor (otherPlayer player) gs) = 0
    | otherwise = -1

-- Part 3: Alpha-Beta pruning algorithm

-- | Marker will be used as an index of all scores in the tree of some rows
type Marker = Int

-- | Take the maximum of a list of minimas of further value and index
maximize :: Rose Int -> (Int,Marker)
maximize tree = (maximum (removeMark (minimaList tree)),maximum (getMark (minimaList tree)))

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

-- | It will return the list of minimas, which is evaluated further in the tree
minimaList :: Rose Int -> [(Int,Marker)]
minimaList tree = case tree of
    RoseNode a [] -> [(a,0)]
    RoseNode _ list -> findMaxs (map maximumList list)

          -- | It will try to evaluate the minimum of the list with some omitting works and mark it with index
    where findMaxs :: [[(Int,Marker)]] -> [(Int,Marker)]
          findMaxs [] = []
          findMaxs (z:zs) = (mayLarge,0): (omitMax 1 mayLarge zs)
              where mayLarge = (minimum (removeMark z))

          -- | it will omit and delete some branches, so it will not be search
          omitMax :: Int -> Int -> [[(Int,Marker)]] -> [(Int,Marker)]
          omitMax marker pot list1 = case list1 of
              [] -> []
              y:ys
                  | decideOmit pot potRest -> omitMax (marker + 1) pot ys
                  | otherwise -> (minimum potRest,marker) : (omitMax (marker + 1) (minimum potRest) ys)
                      where potRest = removeMark y

          -- | This function will judge whether or not it can be omitted  by comparing the potential maximum to next one
          decideOmit :: Int -> [Int] -> Bool
          decideOmit pot1 list2 = case list2 of
              [] -> False
              n:ns
                  | pot1 >= n -> True
                  | otherwise -> decideOmit pot1 ns

-- | it will return the list of maximum, which is evaluated further in the tree
maximumList :: Rose Int -> [(Int,Marker)]
maximumList tree = case tree of
    RoseNode a [] -> [(a,0)]
    RoseNode _ list -> findMins (map minimaList list)

          -- | It will try to evaluate the maximum of the list with some omitting works and mark it with index
    where findMins :: [[(Int,Marker)]] -> [(Int,Marker)]
          findMins [] = []
          findMins (z:zs) = (maySmall,0): (omitMin 0 maySmall zs)
              where maySmall = maximum (removeMark z)

          -- | it will omit and delete some branches, so it will not be search
          omitMin :: Int -> Int -> [[(Int,Marker)]] -> [(Int,Marker)]
          omitMin marker pot list1 = case list1 of
              [] -> []
              y:ys
                  | decideOmit1 pot potRest2 -> omitMin (marker + 1) pot ys
                  | otherwise -> (maximum potRest2,marker + 1) : (omitMin (marker + 1) (maximum potRest2) ys)
                      where potRest2 = removeMark y

          -- | This function will judge whether or not it can be omitted  by comparing the potential minimum to next one
          decideOmit1 ::  Int->  [Int]-> Bool
          decideOmit1 pot1 list2 = case list2 of
              [] -> False
              n:ns
                  | pot1 <= n -> True
                  | otherwise -> decideOmit1 pot1 ns

-- Part 4: Get the Best Card

-- | It will return the Best value so far and marker of a pruning tree
bestMove :: GameState -> (Int,Marker)
bestMove gs = maximize (prune 5 (scoreTree gs))

-- | This Function will cut the tree with the depths or rows that you want
prune :: Int -> Rose a -> Rose a
prune n (RoseNode a list)
    | n == 0 = RoseNode a []
    | otherwise = RoseNode a (map (prune (n-1)) list)

-- | This function will find the best card of next state according to the index(marker)
getBestCard :: (Int,Marker) -> GameState -> Card
getBestCard _ (GameState Finished _ _ _ _) = error "it is finished,can not get best card"
getBestCard (_,marks) state@(GameState (Turn player) _ _ _ _) = wasabiJudge (head (cardsFor player indexState))

          -- | Due to the special case of wasabi, This function will check what kind of wasabi it is added to cards
          -- | and return the what actual card It is
    where wasabiJudge :: Card -> Card
          wasabiJudge card = case card of
              Wasabi (Just (Nigiri int)) -> Nigiri int
              _ -> card
          indexState = (pickSushi state!! marks)

-- | This is the Ai function,which will return the move of taking best card
bestNextMove ::AIFunc
bestNextMove state _ = case gameStatus state of
    Turn _ -> TakeCard (getBestCard (bestMove state) state)
    _ -> error "firstCard: called on finished game"


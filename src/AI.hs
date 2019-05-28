module AI where

import SushiGo

-- | The type of AI functions. Do not change this.
--
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
ais = [("default", firstCard)]

-- Equivalently: firstLegal :: GameState -> Int -> Move
-- firstLegal simply takes the first card it sees
firstCard :: AIFunc
firstCard state _ = case gameStatus state of
  Turn player -> TakeCard (head (handFor player state))
  _ -> error "firstCard: called on finished game"

{--firstTry :: AIFunc
firstTry gs _ = case gameStatus gs of
    Turn player -> TakeCard (getCard (handFor player gs))
        where getCard :: [Card] -> Card
              getCard (x:xs) = case (x:xs) of
                [a] -> a
                Nigiri 3:_ -> Nigiri 3
                Nigiri _:_ -> getCard xs
                Wasabi (Just (Nigiri a)):_ -> Wasabi (Just (Nigiri a))
                Wasabi Nothing:_ -> getCard xs
                Dumplings:_ -> Dumplings
                Eel:_ -> Eel
                Tofu:_ -> getCard xs
                Sashimi:_ -> Sashimi
                [] -> []
                _ -> getCard xs
    _ -> error "firstCard: called on finished game"
--}

newAi :: AIFunc
newAi = undefined


data Rose a = RoseNode a [Rose a]
    deriving (Eq, Show)


pickSushi2 :: GameState -> [GameState]
pickSushi2 gs@(GameState p p1h p1c p2h p2c) = case gs of
    (GameState _ [] p1c [] p2c) -> [(GameState Finished [] p1c [] p2c)]
    (GameState Finished p1h p1c p2h p2c) -> error "game is finished, next moves is not allowed"
    (GameState (Turn player) p1h p1c p2h p2c)
            | player == Player1 -> moves1 p1h
            | otherwise ->  moves2 p2h

    where moves1 :: [Card] -> [GameState]
          moves1 hands = case hands of
            x:xs -> GameState (Turn Player2) (handCanUse (pickCard x p1h p1c)) (cardsChosen (pickCard x p1h p1c)) p2h p2c : moves1 xs
            [] -> []

          moves2 :: [Card] -> [GameState]
          moves2 hs = case hs of
            y:ys -> GameState (Turn Player1) (handCanUse (pickCard y p2h p2c)) p1c p1h (cardsChosen (pickCard y p2h p2c)) : moves2 ys
            [] -> []

handCanUse :: ([Card], [Card]) -> [Card]
handCanUse (a,_) = a

cardsChosen :: ([Card], [Card]) -> [Card]
cardsChosen (_,b) = b

sushiTree :: GameState -> Rose GameState
sushiTree state = RoseNode state (map sushiTree (pickSushi2 state))

won :: Player -> GameState -> Bool
won player gamestate@(GameState p _ _ _ _)
    | scoreCards (cardsFor Player1 gamestate) > scoreCards (cardsFor Player2 gamestate) && p == Finished = True
    | otherwise = False

maximize :: Rose Int -> Int
maximize tree = maximum (maximize' tree)

minimize :: Rose Int -> Int
minimize tree = minimum (minimize' tree)

-- 返回的是，第一个max下面的分支的已经计算好的最小值
maximize' :: Rose Int -> [Int]
maximize' tree = case tree of
    RoseNode a [] -> [a]
    RoseNode _ list -> mapMin (map minimize' list)

-- 提取出来第一个值当作可能的最大值
-- 重新编排这个list
    where mapMin :: [[Int]] -> [Int]
          mapMin (z:zs) = minimum z: (omit (minimum z) zs)
          mapMin [] = []

-- 看是否可以忽略一些元素
          omit :: Int -> [[Int]] -> [Int]
          omit pot list1 = case list1 of
              [] -> []
              y:ys
                  | minlep pot y -> omit pot ys
                  | otherwise -> minimum y : (omit (minimum y) ys)
-- 用来判断的最大的
          minlep :: Int -> [Int] -> Bool
          minlep pot list2 = case list2 of
              [] -> False
              n: ns
                  | pot >= n -> True
                  | otherwise -> minlep pot ns

minimize' :: Rose Int -> [Int]
minimize' tree = case tree of
    RoseNode a [] -> [a]
    RoseNode _ list -> mapMax (map maximize' list)

    where mapMax :: [[Int]] -> [Int]
          mapMax (z:zs) = maximum z: omit (maximum z) zs
          mapMax [] = []

          omit :: Int -> [[Int]] -> [Int]
          omit pot list1 = case list1 of
              [] -> []
              y:ys
                  | maxlep pot y -> omit pot ys
                  | otherwise -> maximum y : (omit (maximum y) ys)

          maxlep ::  Int->  [Int]-> Bool
          maxlep pot list2 = case list2 of
              [] -> False
              n: ns
                  | pot <= n -> True
                  | otherwise -> maxlep pot ns




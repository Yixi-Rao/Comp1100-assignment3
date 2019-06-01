module Main where

import SushiGoTests
import Testing
import AI
import SushiGo



-- | The list of tests to run.
allTests :: Test
allTests = TestGroup "allTests"
  [ sushiGoTests
  , nextStateTests
  , sushiTreeTests
  , alphaBetaTests
  , bestCardTests
  ]

-- | Tests to test whether pickSushi will return the right GameState of defferent players
nextStateTests :: Test
nextStateTests = TestGroup "nextState"
  [ Test "next move player 1 with hands [Nigiri 1,Eel,Tofu]"
    (assertEqual (map (cardsFor Player1) (pickSushi sampleState1))
      ([[Wasabi (Just (Nigiri 1))],[Eel,Wasabi Nothing],[Tofu,Wasabi Nothing]]))

  , Test "next move player 2 with hands [Nigiri 1,Eel,Tofu] "
    (assertEqual (pickSushi sampleState2)
      ([ GameState (Turn Player1) [Eel,Tofu] [] [Dumplings] [Wasabi (Just (Nigiri 1))]
       , GameState (Turn Player1) [Nigiri 1,Tofu] [] [Dumplings] [Eel,Wasabi Nothing]
       , GameState (Turn Player1) [Nigiri 1,Eel] [] [Dumplings] [Tofu,Wasabi Nothing]
       ]))
  ]

  where sampleState1 = GameState (Turn Player1) [Nigiri 1,Eel,Tofu] [Wasabi Nothing] [] []
        sampleState2 = GameState (Turn Player2) [Dumplings] [] [Nigiri 1,Eel,Tofu] [Wasabi Nothing]

-- | This will test on whether the tree is constructed correctly or not by checking each value in the rows
sushiTreeTests :: Test
sushiTreeTests = TestGroup "sushi tree rows tests"
                   (map (treeLevelTest)
                     [ (1,[2,3,-3])
                     , (2,[2,3,6,2,3,6,2,3,6])
                     , (3,[3,4,2,4,2,3,4,5,3,5,3,4,-2,-1,-3,-1,-3,-2])
                     ]
                   )
  where treeLevelTest (levels, expect) = Test ("level " ++ show levels ++ " scores is " ++ show expect )
                                         (assertEqual (levelsTest levels) (expect))

        -- | This function will transform the leaves of GameState to leaves of Score
        leavesScore :: [GameState] -> Int-> [Int]
        leavesScore states row
          | odd row = map (scoreCards) (map (cardsFor Player1) states)
          | otherwise = map (scoreCards) (map (cardsFor Player2) states)

        -- | This will return all the leaves of a tree
        roseLeaves :: Rose a -> [a]
        roseLeaves tree = case tree of
            RoseNode a [] -> [a]
            RoseNode _ list -> foldr (\x y -> concat [roseLeaves x,y]) [] list

        -- | This function will return the scores of a specific row
        levelsTest :: Int -> [Int]
        levelsTest int= leavesScore (roseLeaves (prune int (sushiTree sampleState3))) int

        sampleState3 = (GameState (Turn Player1) [Nigiri 2,Nigiri 3,Eel] [Sashimi] [Sashimi,Nigiri 1,Tofu] [Tofu])

-- | This will test alpha-beta pruning whether it is working in a right way or not
alphaBetaTests :: Test
alphaBetaTests = TestGroup "alpha Beta purning on a Integer tree"
  [ Test "alpha Beta pruning of an Integer Tree form comp1100 week 9 lecture"
    (assertEqual (maximize treeSample)
      ((7,2)))
  , Test "it will omit the branches (second one) that we do not want"
    (assertEqual (elem (2,1) (minimaList treeSample))
      (False))
  ]

-- | This test group will check whether my function will return the best card that I want
bestCardTests :: Test
bestCardTests = TestGroup "get the best card and purn the tree"
  [ Test "prune the tree to depth 2"
    (assertEqual (depthTree (prune 2 treeSample))
      (2))
  , Test "Taking the Nigiri 3 is the best move when hands are all the Nigiris"
    (assertEqual (getBestCard (bestMove wasabiTable) wasabiTable)
      (Nigiri 3))
  , Test "stop the opponent to get the second Tofu"
    (assertEqual (getBestCard (bestMove tofuTable) tofuTable)
      (Tofu))
  , TestGroup "simulate one whole sushigo game with a simple deck of cards"
      (map (getBestCardTest) simulateAGame)]

        -- | This will return the depth of the tree
  where depthTree :: Rose a -> Int
        depthTree tree = case tree of
          RoseNode _ [] -> 0
          RoseNode _ (x:_) -> 1 + depthTree x

        wasabiTable = (GameState (Turn Player1) [Nigiri 1,Nigiri 2,Nigiri 3] [] [Eel,Nigiri 1,Nigiri 2,Nigiri 3] [])
        tofuTable = (GameState (Turn Player1) [Nigiri 1,Tofu] [Nigiri 2] [Nigiri 2,Nigiri 1] [Tofu])
        simulateAGame = [ ( GameState (Turn Player1) [Nigiri 2,Sashimi,Sashimi,Nigiri 3]
                                                    []
                                                    [Nigiri 2,Dumplings,Nigiri 2,Nigiri 1]
                                                    []
                                                    ,Nigiri 3 )
                       , ( GameState (Turn Player1) [Dumplings,Nigiri 2,Nigiri 1]
                                                    [Nigiri 3]
                                                    [Nigiri 2,Sashimi,Sashimi]
                                                    [Nigiri 2]
                                                    ,Nigiri 2 )
                       , ( GameState (Turn Player1) [Sashimi,Sashimi]
                                                    [Nigiri 3,Nigiri 2]
                                                    [Nigiri 1,Dumplings]
                                                    [Nigiri 2,Nigiri 2]
                                                    ,Sashimi )
                       , ( GameState (Turn Player1) [Dumplings]
                                                    [Sashimi,Nigiri 3,Nigiri 2]
                                                    [Sashimi]
                                                    [Nigiri 2,Nigiri 2,Nigiri 1]
                                                    ,Dumplings )
                       ]
        -- | This function will give the best card of a specific game state
        getBestCardTest (gameState, expected) = Test (show gameState ++ " pick the " ++ show expected)
                                                 (assertEqual (getBestCard (bestMove gameState) gameState) (expected))


treeSample :: Rose Int
treeSample = RoseNode 0 [ RoseNode 0 [ RoseNode 0 [ RoseNode 5 [],RoseNode 2 [] ]
                                     , RoseNode 0 [ RoseNode 7 [],RoseNode 6 [] ]
                                     ]
                        , RoseNode 0 [ RoseNode 0 [ RoseNode 1 [],RoseNode 3 [] ]
                                     , RoseNode 0 [ RoseNode 2 [],RoseNode 2 [] ]
                                     ]
                        , RoseNode 0 [ RoseNode 0 [ RoseNode 4 [],RoseNode 7 [] ]
                                     , RoseNode 0 [ RoseNode 9 [],RoseNode 2 [] ]
                                     ]
                        ]

-- | A haskell program starts by running the computation defined by
-- 'main'. We run the tree of tests that we defined above.
main :: IO ()
main = runTests allTests

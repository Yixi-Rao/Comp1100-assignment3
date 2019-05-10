module SushiGoTests where

import ListUtils
import SushiGo
import Testing

-- | Our tests that the Sushi Go rules work correctly.
sushiGoTests :: Test
sushiGoTests = TestGroup "SushiGo"
  [ playMoveTests
  , scoreCardsTests
  , countsTests
  ]

-- | Tests that moves are played correctly.
playMoveTests :: Test
playMoveTests = TestGroup "playMove"
  [ Test "pick one card"
    (assertEqual (playMove (TakeCard Tofu) game)
      (GameState (Turn Player2) [Wasabi Nothing] [Tofu] [Eel, Nigiri 2] []))

  , Test "hands swap after player 2 picks"
    (assertEqual
      (playMove (TakeCard Eel) (playMove (TakeCard Tofu) game))
      (GameState (Turn Player1) [Nigiri 2] [Tofu] [Wasabi Nothing] [Eel]))

  , Test "Nigiri attaches to Wasabi if available"
    (assertEqual
      (playMove (TakeCard (Nigiri 2))
       (playMove (TakeCard Eel)
        (playMove (TakeCard (Wasabi Nothing))
         game)))
      (GameState (Turn Player2) [] [Wasabi (Just (Nigiri 2))] [Tofu] [Eel]))

  , Test "game is finished if no cards are left"
    (assertEqual
      (gameStatus
       (playMove (TakeCard Tofu)
        (playMove (TakeCard (Nigiri 2))
         (playMove (TakeCard Eel)
          (playMove (TakeCard (Wasabi Nothing))
           game)))))
      Finished)

  , Test "using Chopsticks returns them to the hand"
    (assertEqual
      (playMove (UseChopsticks (Nigiri 3)
                 (TakeCard (Wasabi Nothing)))
                chopsticksGame)
      (GameState
         (Turn Player2)
         [Chopsticks]
         -- We took the Nigiri first, so it doesn't attach to the
         -- Wasabi in this instance.
         [Wasabi Nothing, Nigiri 3]
         []
         []))
  ]
  where
    game = initialGame [Wasabi Nothing, Tofu] [Eel, Nigiri 2]
    chopsticksGame
      = GameState
          (Turn Player1)
          [Nigiri 3, Wasabi Nothing]
          [Chopsticks]
          []
          []

-- | Tests that scores are calculated correctly.
scoreCardsTests :: Test
scoreCardsTests = TestGroup "scoreCards"
  [ TestGroup "Nigiri/Wasabi"
    [ Test "is worth its value"
      (assertEqual (scoreCards [Nigiri 3]) 3)

    , Test "is tripled when attached to Wasabi"
      (assertEqual (scoreCards [Wasabi (Just (Nigiri 3))]) 9)

    , Test "unused Wasabi is worth 0"
      (assertEqual (scoreCards [Wasabi Nothing]) 0)
    ]

  , TestGroup "Dumplings"
      (map (testCount Dumplings)
        [ (1, 1)
        , (2, 3)
        , (3, 6)
        , (4, 10)
        , (5, 15)
        , (6, 15)
        , (7, 15)
        ])

  , TestGroup "Eel"
      (map (testCount Eel)
        [ (1, -3)
        , (2, 7)
        , (3, 7)
        ])

  , TestGroup "Tofu"
      (map (testCount Tofu)
        [ (1, 2)
        , (2, 6)
        , (3, 0)
        , (4, 0)
        ])

  , TestGroup "Sashimi"
      (map (testCount Sashimi)
        [ (1, 0)
        , (2, 0)
        , (3, 10)
        , (4, 10)
        , (5, 10)
        , (6, 20)
        , (7, 20)
        , (8, 20)
        ])
  ]
  where
    testCount thing (count, expected)
      = Test (show count ++ " is worth " ++ show expected)
        (assertEqual (scoreCards (replicate count thing)) expected)

-- | Tests for 'counts'.
countsTests :: Test
countsTests = TestGroup "counts"
  [ Test "simple test"
    (assertEqual (counts [Sashimi, Tofu, Sashimi, Eel, Eel, Eel])
      [(Sashimi, 2), (Tofu, 1), (Eel, 3)])
  ]

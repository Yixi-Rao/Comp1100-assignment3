module Main where

import SushiGoTests
import Testing

-- | The list of tests to run. When you define additional test groups,
-- you must list them here or they will not be checked.
--
-- We wrote a number of tests while developing the assignment - they
-- are in SushiGoTests.hs. This leaves AllTests.hs free for your
-- tests. You may want to read them to see how the tests are written,
-- or to get a handle on how the game works.
allTests :: Test
allTests = TestGroup "allTests"
  [ sushiGoTests
  ]

-- | A haskell program starts by running the computation defined by
-- 'main'. We run the tree of tests that we defined above.
main :: IO ()
main = runTests allTests

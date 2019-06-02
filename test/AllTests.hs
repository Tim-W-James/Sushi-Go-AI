module Main where

import SushiGoTests
import Testing
import AI
import SushiGo

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
  , scoreDiffHeuristicTests
  , trimMovesTests
  , removeDuplicatesTests
  , gsMovesTrimmedTests
  , buildTrimmedGSTreeTests
  , alphaBetaTreeValueTests
  , scoreDiffTrimmedValueTests
  , scoreDiffTrimmedRankTests
  , bestRankTests
  , trimmedMinimaxPickTests
  ]

-- | Tests that score difference is being calculated correctly.
scoreDiffHeuristicTests :: Test
scoreDiffHeuristicTests = TestGroup "scoreDiffHeuristic"
  [ Test "inital game"
    (assertEqual (scoreDiffHeuristic initialState5)
      0)

  , Test "mid game"
    (assertEqual (scoreDiffHeuristic midState2)
      3)

  , Test "finished game"
    (assertEqual (scoreDiffHeuristic finishedState3)
      0)
  ]

-- | Tests that moves list is being reduced correctly.
trimMovesTests :: Test
trimMovesTests = TestGroup "trimMoves"
  [ Test "initial game"
    (assertEqual (trimMoves [(Nigiri 1),(Nigiri 1),(Wasabi Nothing),
    Dumplings,Eel,Tofu,Sashimi] initialState1)
      [Wasabi Nothing,Dumplings,Eel,Tofu,Sashimi])

  , Test "mid game"
    (assertEqual (trimMoves [(Nigiri 2),(Nigiri 2),(Wasabi Nothing),(Wasabi Nothing),Dumplings,
    Dumplings,Eel,Eel,Tofu,Tofu,Sashimi,Sashimi] midState1)
      [Nigiri 2,Nigiri 2,Wasabi Nothing,Wasabi Nothing,Dumplings,Dumplings,Sashimi,Sashimi])

  , Test "finished game"
    (assertEqual (trimMoves [] finishedState1)
      [])
  ]

-- | Tests that duplicates are being removed correctly.
removeDuplicatesTests :: Test
removeDuplicatesTests = TestGroup "removeDuplicates"
  [ Test "standard case"
    (assertEqual (removeDuplicates [1,2,3,3,1] :: [Int])
      [1,2,3])

  , Test "empty list"
      (assertEqual (removeDuplicates [] :: [String])
        [])
  ]

-- | Tests that gsMovesTrimmed is being calculated correctly.
gsMovesTrimmedTests :: Test
gsMovesTrimmedTests = TestGroup "gsMovesTrimmed"
  [ Test "initial game"
    (assertEqual (gsMovesTrimmed initialState3)
      [GameState (Turn Player2) [Nigiri 1] [Dumplings] [Nigiri 2,Eel] []])

  , Test "mid game"
    (assertEqual (gsMovesTrimmed midState2)
      [GameState (Turn Player1) [Nigiri 2,Eel] [Sashimi,Sashimi] [Nigiri 1,Sashimi] [Nigiri 2,Eel],
      GameState (Turn Player1) [Nigiri 2,Nigiri 2] [Sashimi,Sashimi] [Nigiri 1,Sashimi] [Eel,Eel]])

  , Test "finished game"
    (assertEqual (gsMovesTrimmed finishedState1)
      [GameState Finished [] [Sashimi,Sashimi,Sashimi] [] [Eel,Eel,Eel]])
  ]

-- | Tests that buildTrimmedGSTree is being calculated correctly.
buildTrimmedGSTreeTests :: Test
buildTrimmedGSTreeTests = TestGroup "buildTrimmedGSTree"
  [ Test "initial game"
    (assertEqual (buildTrimmedGSTree initialState4 2)
      (GSTree (GameState (Turn Player1) [Dumplings] [] [Eel] []) [GSTree
      (GameState (Turn Player2) [] [Dumplings] [Eel] []) [GSTree
      (GameState Finished [] [Dumplings] [] [Eel]) []]]))

  , Test "mid game"
    (assertEqual (buildTrimmedGSTree midState4 3)
      (GSTree (GameState (Turn Player1) [Eel,Eel] [] [Sashimi,Sashimi] [])
      [GSTree (GameState (Turn Player2) [Eel] [Eel] [Sashimi,Sashimi] [])
      [GSTree (GameState (Turn Player1) [Sashimi] [Eel] [Eel] [Sashimi])
      [GSTree (GameState (Turn Player2) [] [Sashimi,Eel] [Eel] [Sashimi]) []]]]))

  , Test "finished game"
    (assertEqual (buildTrimmedGSTree finishedState1 0)
      (GSTree (GameState Finished [] [Sashimi,Sashimi,Sashimi] [] [Eel,Eel,Eel]) []))
  ]

-- | Tests that alphaBetaTreeValue is being calculated correctly.
alphaBetaTreeValueTests :: Test
alphaBetaTreeValueTests = TestGroup "alphaBetaTreeValue"
  [ Test "initial game"
    (assertEqual (alphaBetaTreeValue (-10000) 10000
    scoreDiffHeuristic (buildTrimmedGSTree initialState5 9))
      4.0)

  , Test "mid game"
    (assertEqual (alphaBetaTreeValue (-10000) 10000
    scoreDiffHeuristic (buildTrimmedGSTree midState1 5))
      (-4.0))

  , Test "finished game"
    (assertEqual (alphaBetaTreeValue (-10000) 10000
    scoreDiffHeuristic (buildTrimmedGSTree finishedState2 1))
      (-3.0))
  ]


-- | Tests that scoreDiffTrimmedValue is being calculated correctly.
scoreDiffTrimmedValueTests :: Test
scoreDiffTrimmedValueTests = TestGroup "scoreDiffTrimmedValue"
  [ Test "initial game"
    (assertEqual (scoreDiffTrimmedValue initialState3 5)
      (-5.0))

  , Test "mid game"
    (assertEqual (scoreDiffTrimmedValue midState1 2)
      (-2.0))

  , Test "finished game"
    (assertEqual (scoreDiffTrimmedValue finishedState1 5)
      3.0)
  ]

-- | Tests that scoreDiffTrimmedRank is being calculated correctly.
scoreDiffTrimmedRankTests :: Test
scoreDiffTrimmedRankTests = TestGroup "scoreDiffTrimmedRank"
  [ Test "initial game"
    (assertEqual (scoreDiffTrimmedRank initialState1 4)
      [(Wasabi Nothing,1.0),(Dumplings,0.0),(Eel,1.0),(Tofu,0.0),(Sashimi,-1.0)])

  , Test "mid game"
    (assertEqual (scoreDiffTrimmedRank midState1 4)
      [(Wasabi Nothing,-4.0),(Dumplings,-7.0),(Sashimi,-7.0)])

  , Test "finished game"
    (assertEqual (scoreDiffTrimmedRank finishedState2 0)
      [])
  ]

-- | Tests that bestRank is being calculated correctly.
bestRankTests :: Test
bestRankTests = TestGroup "bestRank"
  [ Test "player 1"
    (assertEqual (bestRank Player1 [(Eel,1),(Dumplings,5),(Sashimi,3)])
      Dumplings)

  , Test "player 2"
    (assertEqual (bestRank Player2 [(Eel,1),(Dumplings,5),(Sashimi,3)])
      Eel)
  ]

-- | Tests that trimmedMinimaxPick is being calculated correctly.
trimmedMinimaxPickTests :: Test
trimmedMinimaxPickTests = TestGroup "trimmedMinimaxPick"
  [ Test "initial game"
    (assertEqual (trimmedMinimaxPick initialState2 3)
      (TakeCard Tofu))

  , Test "mid game"
    (assertEqual (trimmedMinimaxPick midState5 5)
      (TakeCard Sashimi))
  ]

-- | A haskell program starts by running the computation defined by
-- 'main'. We run the tree of tests that we defined above.
main :: IO ()
main = runTests allTests

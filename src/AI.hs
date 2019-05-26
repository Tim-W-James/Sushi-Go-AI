{-|
Module      : src\AI.hs
Description : Defines a set of AIs and associated functions.
Maintainer  : u6947396@anu.edu.au
-}

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

-- | Rose tree for use with GameStates
data GSTree a = GSTree a [GSTree a]

-- ^ Prints a clean, readable GSTree
-- code taken from lecture material (7th May)
instance Show a => Show (GSTree a) where
    show tree = pretty tree
        where
          pretty :: Show a => GSTree a -> String
          pretty = unlines.layout
          layout :: Show a => GSTree a -> [String]
          layout (GSTree v []) = [show v]
          layout (GSTree v children) = [show v] ++ concat (map indent (map layout children))
          indent :: [String] -> [String]
          indent = map ("  "++)

{-
 Useful GameStates for testing ranking and AI.
-}

-- first turn GameStates
initialState1 :: GameState
initialState1 =
  initialGame [(Nigiri 1),(Nigiri 1),(Wasabi Nothing),Dumplings,Eel,Tofu,Sashimi]
  [(Nigiri 2),(Nigiri 2),(Wasabi Nothing),Dumplings,Eel,Tofu,Sashimi]

initialState2 :: GameState
initialState2 =
  initialGame [(Nigiri 1),Dumplings,Tofu] [(Nigiri 2),Eel,Sashimi]

initialState3 :: GameState
initialState3 =
  initialGame [(Nigiri 1),Dumplings] [(Nigiri 2),Eel]

initialState4 :: GameState
initialState4 =
  initialGame [Dumplings] [Eel]

initialState5 :: GameState
initialState5 =
  initialGame [(Nigiri 1),(Nigiri 1),(Wasabi Nothing),Dumplings,Eel,Tofu,Sashimi,(Nigiri 3),
  (Nigiri 2),(Wasabi Nothing),Dumplings,Eel,Tofu,Sashimi,Tofu] [(Nigiri 2),(Nigiri 2),
  (Wasabi Nothing),Dumplings,Eel,Tofu,Sashimi,(Nigiri 3),(Nigiri 1),(Wasabi Nothing),
  Dumplings,Eel,Tofu,Sashimi,Sashimi]

-- GameStates that are partially complete
midState1 :: GameState
midState1 =
  GameState (Turn Player1) [(Nigiri 1),(Nigiri 1),(Wasabi Nothing),(Wasabi Nothing),
  Dumplings,Dumplings,Eel,Eel,Tofu,Tofu,Sashimi,Sashimi] [(Nigiri 3),Eel,Sashimi,Sashimi]
  [(Nigiri 2),(Nigiri 2),(Wasabi Nothing),(Wasabi Nothing),Dumplings,Dumplings,Eel,Eel,Tofu,
  Tofu,Sashimi,Sashimi] [(Nigiri 3),Eel,Eel,Sashimi]

midState2 :: GameState
midState2 =
  GameState (Turn Player2) [(Nigiri 1),Sashimi] [Sashimi,Sashimi] [(Nigiri 2),(Nigiri 2),Eel] [Eel]

midState3 :: GameState
midState3 =
  GameState (Turn Player1) [(Nigiri 2)] [] [(Nigiri 1)] []

-- GameStates that are finished
finishedState1 :: GameState
finishedState1 =
  GameState Finished [] [Sashimi,Sashimi,Sashimi] [] [Eel,Eel,Eel]

finishedState2 :: GameState
finishedState2 =
  GameState Finished [] [Eel,Eel,Eel] [] [Sashimi,Sashimi,Sashimi]

finishedState3 :: GameState
finishedState3 =
  GameState Finished [] [(Nigiri 1)] [] [(Nigiri 1)]

-- | The table of all AIs you have implemented. We will mark the AI
-- called "default" as your submission, but you may include other AIs
-- for testing.
ais :: [(String, AIFunc)]
ais =
  [("firstLegal", firstPick),
  ("greedy", greedyPick),
  ("partialMinimax",partialMinimaxPick),
  ("minimax",minimaxPick),
  ("default",alphaBetaMinimaxPick)]

{-
 AIs.
-}

-- Equivalently: firstLegal :: GameState -> Int -> Move
-- firstLegal simply takes the first card it sees
firstPick :: AIFunc
firstPick state _ = case gameStatus state of
  Turn player -> TakeCard (head (handFor player state))
  _ -> error "firstPick: called on finished game"

-- greedy AI that picks the card with the greatest immediate increase in score
greedyPick :: AIFunc
greedyPick state _ = case gameStatus state of
  Turn _ -> TakeCard (maxRank (greedyRank state) (head (greedyRank state)))
  _ -> error "greedyPick: called on finished game"

-- minimax AI (not pruned) limited to a lookahead of 3
partialMinimaxPick :: AIFunc
partialMinimaxPick state _ = case gameStatus state of
  Turn player -> TakeCard (bestRank player (scoreDiffHeuristicRank state 3))
  _ -> error "partialMinimaxPick: called on finished game"

-- minimax AI (not pruned) with highest lookahead possible
minimaxPick :: AIFunc
minimaxPick state lookahead = case gameStatus state of
  Turn player -> TakeCard (bestRank player (scoreDiffHeuristicRank state lookahead))
  _ -> error "minimaxPick: called on finished game"

-- minimax AI (pruned) with highest lookahead possible
alphaBetaMinimaxPick :: AIFunc
alphaBetaMinimaxPick state lookahead = case gameStatus state of
  Turn player -> TakeCard (bestRank player (scoreDiffABHeuristicRank state lookahead))
  _ -> error "alphaBetaMinimaxPick: called on finished game"

{-
 Ranking functions which associate each pick with a value.
-}

-- | Find the best rank for the relevant player from a list of weighted picks
-- where max is Player1 and min is Player2
bestRank :: Player -> [(Card,Int)] -> Card
bestRank player handOptions = case handOptions of
  x:xs
    | player == Player1 -> maxRank xs x
    | otherwise -> minRank xs x
  _ -> error "greedyGreatest: hand is empty"

-- | Find max rank from a list of weighted picks
maxRank :: [(Card,Int)] -> (Card,Int) -> Card
maxRank options (currCard,currScore) = case options of
  [] -> currCard
  (card,score):xs
    | score > currScore -> maxRank xs (card,score)
    | otherwise -> maxRank xs (currCard,currScore)

-- | Find min rank from a list of weighted picks
minRank :: [(Card,Int)] -> (Card,Int) -> Card
minRank options (currCard,currScore) = case options of
  [] -> currCard
  (card,score):xs
    | score < currScore -> minRank xs (card,score)
    | otherwise -> minRank xs (currCard,currScore)

-- | Generate greedy ranks for possible picks from a GameState
greedyRank :: GameState -> [(Card,Int)]
greedyRank (GameState s p1h p1c p2h p2c) = case s of
  Turn Player1 -> zip p1h (map scoreCards (map (++p1c) (map listify p1h)))
  _ -> zip p2h (map scoreCards (map (++p2c) (map listify p2h)))
  where
    listify :: a -> [a]
    listify x = [x]

-- | Generate (unpruned) heuristic ranks from a tree of GameStates based on score difference
scoreDiffHeuristicRank :: GameState -> Int -> [(Card,Int)]
scoreDiffHeuristicRank gs@(GameState s p1h _ p2h _) lookahead = case s of
  Turn Player1 -> zip p1h (map (`scoreDiffValue` lookahead) (gsMoves gs))
  _ -> zip p2h (map (`scoreDiffValue` lookahead) (gsMoves gs))

-- | Generate (alpha-beta pruned) heuristic ranks from a tree of GameStates based on score difference
scoreDiffABHeuristicRank :: GameState -> Int -> [(Card,Int)]
scoreDiffABHeuristicRank gs@(GameState s p1h _ p2h _) lookahead = case s of
  Turn Player1 -> zip p1h (map (`scoreDiffABValue` lookahead) (gsMoves gs))
  _ -> zip p2h (map (`scoreDiffABValue` lookahead) (gsMoves gs))

{-
 Functions to help calculate ranks.
-}

-- | Finds the resulting possible GameStates of a pick from a given GameState
gsMoves :: GameState -> [GameState]
gsMoves gameState@(GameState s p1h _ p2h _) = case s of
  Turn Player1 -> map (`playMove` gameState) (map TakeCard p1h)
  Turn Player2 -> map (`playMove` gameState) (map TakeCard p2h)
  _ -> [gameState]

-- | Build a full tree of possible GameStates
buildGSTreeFull :: GameState -> GSTree GameState
buildGSTreeFull gs = case gameStatus gs of
  Finished -> GSTree gs []
  _ -> GSTree gs (map buildGSTreeFull (gsMoves gs))

-- | Build a partial tree of possible GameStates with a depth of lookahead
buildGSTree :: GameState -> Int -> GSTree GameState
buildGSTree gameState lookahead = buildGSTreeHelper 0 lookahead gameState
  where
    buildGSTreeHelper :: Int -> Int -> GameState -> GSTree GameState
    buildGSTreeHelper currDepth maxDepth gs
      | currDepth < maxDepth && gameStatus gs /= Finished =
        GSTree gs (map (buildGSTreeHelper (currDepth + 1) maxDepth) (gsMoves gs))
        -- build until max depth or Finished GameStatus is reached
      | otherwise =
        GSTree gs []

-- | Score difference for heuristics
scoreDiff :: GameState -> Int
scoreDiff (GameState _ _ p1c _ p2c) = scoreCards p1c - scoreCards p2c

-- | Assign a value to a given GameState based on score difference
-- where max is Player1 and min is Player2
scoreDiffValue :: GameState -> Int -> Int
scoreDiffValue gameState lookahead = scoreDiffGSValueHelper (buildGSTree gameState lookahead)
  where
    scoreDiffGSValueHelper :: GSTree GameState -> Int
    scoreDiffGSValueHelper gsTree = case gsTree of
      GSTree gs [] -> scoreDiff gs
      GSTree gs list -> case gameStatus gs of
          Turn Player1 -> maximum (map scoreDiffGSValueHelper list)
          Turn Player2 -> minimum (map scoreDiffGSValueHelper list)
          _ -> error "scoreDiffValue: game finished"

-- | Assign a value to a given GameState based on score difference with alpha-beta pruning
-- where max is Player1 and min is Player2
scoreDiffABValue :: GameState -> Int -> Int
scoreDiffABValue gameState lookahead = scoreDiffABValueHelper (-10000) 10000 (buildGSTree gameState lookahead)
  where
    scoreDiffABValueHelper :: Int -> Int -> GSTree GameState -> Int
    scoreDiffABValueHelper alpha beta gsTree = case gsTree of
      GSTree gs [] -> scoreDiff gs
      GSTree gs list@(x:_) -> case gameStatus gs of
        Turn Player1 -> maxSearch (scoreDiffABValueHelper alpha beta x) list alpha beta
        Turn Player2 -> minSearch (scoreDiffABValueHelper alpha beta x) list alpha beta
        _ -> error "scoreDiffValue: game finished"

    -- prune beta
    maxSearch :: Int -> [GSTree GameState] -> Int -> Int -> Int
    maxSearch value list a b
      | maximum [a,value] >= b = maximum [a,value]
      | otherwise = (maximum (map (scoreDiffABValueHelper (maximum [a,value]) b) list))

    -- prune alpha
    minSearch :: Int -> [GSTree GameState] -> Int -> Int -> Int
    minSearch value list a b
      | a >= minimum [b,value] = minimum [b,value]
      | otherwise = (minimum (map (scoreDiffABValueHelper a (minimum [b,value])) list))
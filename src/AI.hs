{-|
Module      : src\AI.hs
Description : Defines a set of AIs and associated functions.
Maintainer  : u6947396@anu.edu.au
-}

module AI where

import SushiGo
import ListUtils

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

-- | The table of all AIs you have implemented. We will mark the AI
-- called "default" as your submission, but you may include other AIs
-- for testing.
ais :: [(String, AIFunc)]
ais =
  [("firstLegal", firstPick),
  ("greedy", greedyPick),
  ("partialMinimax",partialMinimaxPick),
  ("minimax",minimaxPick),
  ("alphaBetaMinimax",alphaBetaMinimaxPick),
  ("default",trimmedMinimaxPick),
  ("potentialTrimmedMinimax",potentialTrimmedMinimaxPick)]

{-=================================
 Functions used for the default AI.
=================================-}

{-
 First we need to decide which moves to consider.
 I call this trimming, and it takes place before alpha-beta pruning.
 Information may be lost in this process,
 however it drastically speeds up calculation times.
-}

-- | Strategically trims a list of gameStates,
-- taking optimal moves and discarding poor decisions
trimMoves :: [Card] -> GameState -> [Card]
trimMoves possibleMoves gameState = case gameStatus gameState of
  Turn player -- moves the AI should always take
    | findRank Sashimi playerHand == 1 &&
      findRank Sashimi enemyHand < 1 &&
      (findRank Sashimi enemyCards + 1 `mod` 3 == 0 ||
      findRank Sashimi playerCards + 1 `mod` 3 == 0) -> [Sashimi]
    | findRank (Nigiri 3) playerHand > 0 &&
      (findRank (Wasabi Nothing) enemyCards > 0 ||
      findRank (Wasabi Nothing) playerCards > 0) -> [(Nigiri 3)]
    | otherwise -> trimWorstMoves possibleMoves (length possibleMoves)
      where
        -- functions to get useful information from a gameState
        playerHand = counts (handFor player gameState)
        playerCards = counts (cardsFor player gameState)
        enemyHand = counts (handFor (otherPlayer player) gameState)
        enemyCards = counts (cardsFor (otherPlayer player) gameState)
        obtainableCards =
          counts ((handFor (otherPlayer player) gameState)
          ++ (handFor player gameState))

        -- moves that should be avoided unless necessary
        trimWorstMoves :: [Card] -> Int -> [Card]
        trimWorstMoves moves optionSize
          | optionSize > 1 = case moves of
            [] -> []
            Tofu:xs
              | findRank Tofu obtainableCards > 3 ||
                findRank Tofu playerCards == 2 -> trimWorstMoves xs (optionSize - 1)
              | otherwise -> Tofu : trimWorstMoves xs optionSize
            Eel:xs
              | findRank Eel obtainableCards > 3 -> trimWorstMoves xs (optionSize - 1)
              | otherwise -> Eel : trimWorstMoves xs optionSize
            (Nigiri 1):xs
              | findRank (Nigiri 2) playerHand > 0 ||
                playerHandNigiri3 > 0 ||
                (playerHandDumplings > 0 &&
                playerCardsDumplings < 5) -> trimWorstMoves xs (optionSize - 1)
              | otherwise -> (Nigiri 1) : trimWorstMoves xs optionSize
            (Nigiri 2):xs
              | playerHandNigiri3 > 0 ||
                (playerHandDumplings > 0 &&
                playerCardsDumplings < 5 &&
                playerCardsDumplings > 0) -> trimWorstMoves xs (optionSize - 1)
              | otherwise -> (Nigiri 2) : trimWorstMoves xs optionSize
            (Nigiri 3):xs
              | playerHandDumplings > 0 &&
                playerCardsDumplings < 5 &&
                playerCardsDumplings > 1 -> trimWorstMoves xs (optionSize - 1)
              | otherwise -> (Nigiri 3) : trimWorstMoves xs optionSize
            x:xs -> x : trimWorstMoves xs optionSize
          | otherwise = moves
            where
              playerHandNigiri3 = findRank (Nigiri 3) playerHand
              playerHandDumplings = findRank Dumplings playerHand
              playerCardsDumplings = findRank Dumplings playerCards
  _ -> []

-- | Remove any duplicates from a list
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates list = case list of
  [] -> []
  (x:xs) -> x : removeDuplicates (filter (/=x) xs)

-- | Finds the resulting possible GameStates of a pick from a given GameState,
-- after being trimmed extensively
gsMovesTrimmed :: GameState -> [GameState]
gsMovesTrimmed gameState@(GameState s p1h _ p2h _) = case s of
  Turn Player1 ->
    map (`playMove` gameState) (map TakeCard (trimMoves (removeDuplicates p1h) gameState))
  Turn Player2 ->
    map (`playMove` gameState) (map TakeCard (trimMoves (removeDuplicates p2h) gameState))
  _ -> [gameState]

{-
 With a list of possible moves we need to consider,
 we can then generate a game tree of these moves.
-}

-- | Build a partial tree of possible GameStates with a depth of lookahead
buildTrimmedGSTree :: GameState -> Int -> GSTree GameState
buildTrimmedGSTree gameState lookahead = buildTrimmedGSTreeHelper 0 lookahead gameState
  where
    buildTrimmedGSTreeHelper :: Int -> Int -> GameState -> GSTree GameState
    buildTrimmedGSTreeHelper currDepth maxDepth gs
      | currDepth < maxDepth && gameStatus gs /= Finished =
        GSTree gs (map (buildTrimmedGSTreeHelper (currDepth + 1) maxDepth) (gsMovesTrimmed gs))
        -- build until max depth or Finished GameStatus is reached
      | otherwise =
        GSTree gs []

{-
 We then begin assigning values to each node of the tree with a score difference heuristic,
 and discard parts of the tree with alpha-beta pruning.
 Using minimax, we find which choice is best for Player1 with the maximum value, and which is
 best for Player2 with a minimum value.
-}

-- | Simple heuristic for a score difference
-- where max is Player1 and min is Player2
scoreDiffHeuristic :: GameState -> Double
scoreDiffHeuristic (GameState _ _ p1c _ p2c) = fromIntegral (scoreCards p1c - scoreCards p2c)

-- | Assign a value to a given GameState based on score difference and a lookahead,
-- with alpha-beta pruning, where max is Player1 and min is Player2
alphaBetaTreeValue :: Double -> Double -> (GameState -> Double) -> GSTree GameState -> Double
alphaBetaTreeValue alpha beta heuristic gsTree = case gsTree of
  GSTree gs [] -> heuristic gs
  GSTree gs list@(x:_) -> case gameStatus gs of
    Turn Player1 -> maxSearch (alphaBetaTreeValue alpha beta heuristic x) list alpha beta
    Turn Player2 -> minSearch (alphaBetaTreeValue alpha beta heuristic x) list alpha beta
    _ -> error "scoreDiffValue: game finished"

    where
        -- prune beta
        maxSearch :: Double -> [GSTree GameState] -> Double -> Double -> Double
        maxSearch value gsList a b
          | maxA >= b = maxA
          | otherwise = (maximum (map (alphaBetaTreeValue maxA b heuristic) gsList))
          where
            maxA = maximum [a,value]

        -- prune alpha
        minSearch :: Double -> [GSTree GameState] -> Double -> Double -> Double
        minSearch value gsList a b
          | a >= minB = minB
          | otherwise = (minimum (map (alphaBetaTreeValue a minB heuristic) gsList))
          where
            minB = minimum [b,value]

-- | Assign a value to a given GameState based on score difference
-- and a lookahead with extensive pruning.
-- where max is Player1 and min is Player2
scoreDiffTrimmedValue :: GameState -> Int -> Double
scoreDiffTrimmedValue gameState lookahead =
  alphaBetaTreeValue (-10000) 10000 scoreDiffHeuristic (buildTrimmedGSTree gameState lookahead)

{-
 We zip a list of values with their associated pick,
 then figure out which choice is best depending on which player the AI is.
-}

-- | Generate (extensively pruned) ranks from a tree of GameStates based on score difference
scoreDiffTrimmedRank :: GameState -> Int -> [(Card,Double)]
scoreDiffTrimmedRank gs@(GameState s p1h _ p2h _) lookahead = case s of
  Turn Player1 -> zip (trimMoves (removeDuplicates p1h) gs) (map (`scoreDiffTrimmedValue` lookahead) (gsMovesTrimmed gs))
  _ -> zip (trimMoves (removeDuplicates p2h) gs) (map (`scoreDiffTrimmedValue` lookahead) (gsMovesTrimmed gs))

-- | DEFAULT AI: minimax AI (extended pruning) with score difference heuristic
-- If hand is smaller than 6 cards, there is enough time to reach
-- the bottom of the game tree with alpha beta minimax.
-- This is done because there is no need to risk information loss
-- when perfect information can be achieved within the time limit.
trimmedMinimaxPick :: AIFunc
trimmedMinimaxPick state lookahead = case gameStatus state of
  Turn player
    | length (handFor player state) < 6 -> TakeCard (bestRank player (scoreDiffABRank state lookahead))
    | otherwise -> TakeCard (bestRank player (scoreDiffTrimmedRank state lookahead))
  _ -> error "trimmedMinimaxPick: called on finished game"

{-
 Useful functions for dealing with tuples.
-}

-- | Find the best rank for the relevant player from a list of weighted picks
-- where max is Player1 and min is Player2
bestRank :: Player -> [(Card,Double)] -> Card
bestRank player handOptions = case handOptions of
  x:xs
    | player == Player1 -> maxRank xs x
    | otherwise -> minRank xs x
  _ -> error "greedyGreatest: hand is empty"

-- | Find max rank from a list of weighted picks
maxRank :: [(Card,Double)] -> (Card,Double) -> Card
maxRank options (currCard,currScore) = case options of
  [] -> currCard
  (card,score):xs
    | score > currScore -> maxRank xs (card,score)
    -- if equal, prefer some cards over others
    | score == currScore && (currCard == Sashimi ||
      currCard == Tofu) -> maxRank xs (card,score)
    | otherwise -> maxRank xs (currCard,currScore)

-- | Find min rank from a list of weighted picks
minRank :: [(Card,Double)] -> (Card,Double) -> Card
minRank options (currCard,currScore) = case options of
  [] -> currCard
  (card,score):xs
    | score < currScore -> minRank xs (card,score)
    -- if equal, prefer some cards over others
    | score == currScore && (currCard == Sashimi ||
      currCard == Tofu) -> maxRank xs (card,score)
    | otherwise -> minRank xs (currCard,currScore)

-- | Find the rank of a given card from a list of weighted picks
findRank :: Card -> [(Card,Int)] -> Int
findRank x list = case list of
  [] -> 0
  (card,score):xs
    | card == x -> score
    | otherwise -> findRank x xs

{-====================================================
 Functions used in other AIs, useful for benchmarking.
 Note that alphaBetaMinimax AI is used in some
 situations with the default AI.
====================================================-}

{-
 Ranking functions which associate each pick with a value.
-}

-- | Generate greedy ranks for possible picks from a GameState
greedyRank :: GameState -> [(Card,Double)]
greedyRank (GameState s p1h p1c p2h p2c) = case s of
  Turn Player1 -> zip p1h (map (fromIntegral . scoreCards) (map (++p1c) (map listify p1h)))
  _ -> zip p2h (map (fromIntegral . scoreCards) (map (++p2c) (map listify p2h)))
  where
    listify :: a -> [a]
    listify x = [x]

-- | Generate (unpruned) ranks from a tree of GameStates based on score difference
scoreDiffRank :: GameState -> Int -> [(Card,Double)]
scoreDiffRank gs@(GameState s p1h _ p2h _) lookahead = case s of
  Turn Player1 -> zip p1h (map (`scoreDiffValue` lookahead) (gsMoves gs))
  _ -> zip p2h (map (`scoreDiffValue` lookahead) (gsMoves gs))

-- | Generate (alpha-beta pruned) ranks from a tree of GameStates based on score difference
scoreDiffABRank :: GameState -> Int -> [(Card,Double)]
scoreDiffABRank gs@(GameState s p1h _ p2h _) lookahead = case s of
  Turn Player1 -> zip (removeDuplicates p1h) (map (`scoreDiffABValue` lookahead) (gsMoves gs))
  _ -> zip (removeDuplicates p2h) (map (`scoreDiffABValue` lookahead) (gsMoves gs))

-- | Generate (extensively pruned) ranks from a tree of GameStates based on score potential
potentialTrimmedRank :: GameState -> Int -> [(Card,Double)]
potentialTrimmedRank gs@(GameState s p1h _ p2h _) lookahead = case s of
  Turn Player1 ->
    zip (trimMoves (removeDuplicates p1h) gs)
    (map (`scorePotentialTrimmedValue` lookahead) (gsMovesTrimmed gs))
  _ ->
    zip (trimMoves (removeDuplicates p2h) gs)
    (map (`scorePotentialTrimmedValue` lookahead) (gsMovesTrimmed gs))

{-
 Functions to help calculate ranks.
-}

-- | Finds the resulting possible GameStates of a pick from a given GameState
gsMoves :: GameState -> [GameState]
gsMoves gameState@(GameState s p1h _ p2h _) = case s of
  Turn Player1 -> map (`playMove` gameState) (map TakeCard (removeDuplicates p1h))
  Turn Player2 -> map (`playMove` gameState) (map TakeCard (removeDuplicates p2h))
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

-- | Heuristic for a potential score difference
-- where max is Player1 and min is Player2
scorePotentialHeuristic :: GameState -> Double
scorePotentialHeuristic gs@(GameState _ _ p1c _ p2c) =
  scorePotentialCards p1c gs - scorePotentialCards p2c gs

-- | Scores a list of cards with a potential value added
scorePotentialCards:: [Card] -> GameState -> Double
scorePotentialCards cards gs = case gameStatus gs of
  Turn player -> sum
    [ scoreSingleCards
    , scoreDumplings (countHelper Dumplings)
    , scoreEel (countHelper Eel)
    , scoreTofu (countHelper Tofu)
    , scoreSashimi (countHelper Sashimi)
    ]
      where
        scoreSingleCards = sum (flip map cards
          (\card -> case card of
              Nigiri n -> fromIntegral n
              Wasabi Nothing
                | findRank (Nigiri 3) obtainableCards > 1 -> 4.5
                | findRank (Nigiri 2) obtainableCards > 1 -> 3
                | findRank (Nigiri 1) obtainableCards > 1 -> 1.5
                | otherwise -> 0
              Wasabi (Just (Nigiri n)) -> fromIntegral (n * 3)
              -- Technically redundant, but we spell it out so every card
              -- type is named in the scoring function.
              Chopsticks -> 0
              _ -> 0))


        -- scoreDumplings accounts for any additional Dumplings
        -- in the GameState which may still be obtainable
        scoreDumplings n
          | n == 0 = 0
          | n == 1 && obtainableDumplings == 2 = 1.5
          | n == 1 && obtainableDumplings == 3 = 2
          | n == 1 && obtainableDumplings == 4 = 2.5
          | n == 1 && obtainableDumplings > 4 = 3
          | n == 1 = 1
          | n == 2 && obtainableDumplings == 2 = 4.5
          | n == 2 && obtainableDumplings == 3 = 5.3
          | n == 2 && obtainableDumplings > 3 = 6
          | n == 2 = 3
          | n == 3 && obtainableDumplings == 2 = 8
          | n == 3 && obtainableDumplings > 2 = 9
          | n == 3 = 6
          | n == 4 && obtainableDumplings > 1 = 12.5
          | n == 4 = 10
          | n >= 5 = 15
          | otherwise = error "scoreDumplings: negative Dumpling count?"
            where
              obtainableDumplings = findRank Dumplings obtainableCards

        scoreEel n
          | n == 0 = 0
          | n == 1 && findRank Eel obtainableCards > 1 = 3.5 -- potential to be worth 3.5 points
          | n == 1 = -3
          | n >= 2 = 7
          | otherwise = error "scoreEel: negative Eel count?"

        scoreTofu n
          | n == 0 = 0
          | n == 1 && obtainableTofu > 1 = 3 -- potential to be worth 3 points
          | n == 1 = 2
          | n == 2 && obtainableTofu > 0 = 3 -- avoid risk of getting > 3 Tofu
          | n == 2 = 6
          | n >= 3 = 0
          | otherwise = error "scoreTofu: negative Tofu count?"
            where
              obtainableTofu = findRank Tofu obtainableCards

        scoreSashimi n
          | findRank Sashimi obtainableCards > 3 = 3.3 -- potential to be worth 3.3 points each
          | otherwise = fromIntegral (n `div` 3 * 10)

        countHelper card = length (filter (== card) cards)

        obtainableCards = counts ((handFor (otherPlayer player) gs) ++ (handFor player gs))

  _ -> fromIntegral (scoreCards cards)

-- | Assign a value to a given GameState based on score difference and a lookahead
-- where max is Player1 and min is Player2
scoreDiffValue :: GameState -> Int -> Double
scoreDiffValue gameState lookahead = scoreDiffGSValueHelper (buildGSTree gameState lookahead)
  where
    scoreDiffGSValueHelper :: GSTree GameState -> Double
    scoreDiffGSValueHelper gsTree = case gsTree of
      GSTree gs [] -> scoreDiffHeuristic gs
      GSTree gs list -> case gameStatus gs of
          Turn Player1 -> maximum (map scoreDiffGSValueHelper list)
          Turn Player2 -> minimum (map scoreDiffGSValueHelper list)
          _ -> error "scoreDiffValue: game finished"

-- | Assign a value to a given GameState based on
-- score difference and a lookahead with alpha-beta pruning
-- where max is Player1 and min is Player2
scoreDiffABValue :: GameState -> Int -> Double
scoreDiffABValue gameState lookahead =
  alphaBetaTreeValue (-10000) 10000 scoreDiffHeuristic (buildGSTree gameState lookahead)

-- | Assign a value to a given GameState based on score potential
-- and a lookahead with extensive pruning
-- where max is Player1 and min is Player2
scorePotentialTrimmedValue :: GameState -> Int -> Double
scorePotentialTrimmedValue gameState lookahead =
  alphaBetaTreeValue (-10000) 10000 scorePotentialHeuristic (buildTrimmedGSTree gameState lookahead)


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

-- minimax AI (not pruned) limited to a lookahead of 3 with score difference heuristic
partialMinimaxPick :: AIFunc
partialMinimaxPick state _ = case gameStatus state of
  Turn player -> TakeCard (bestRank player (scoreDiffRank state 3))
  _ -> error "partialMinimaxPick: called on finished game"

-- minimax AI (not pruned) with highest lookahead possible with score difference heuristic
minimaxPick :: AIFunc
minimaxPick state lookahead = case gameStatus state of
  Turn player -> TakeCard (bestRank player (scoreDiffRank state lookahead))
  _ -> error "minimaxPick: called on finished game"

-- minimax AI (pruned) with highest lookahead possible with score difference heuristic
alphaBetaMinimaxPick :: AIFunc
alphaBetaMinimaxPick state lookahead = case gameStatus state of
  Turn player -> TakeCard (bestRank player (scoreDiffABRank state lookahead))
  _ -> error "alphaBetaMinimaxPick: called on finished game"

-- minimax AI (extended pruning) with highest lookahead possible with predictive heuristic
potentialTrimmedMinimaxPick :: AIFunc
potentialTrimmedMinimaxPick state lookahead = case gameStatus state of
  Turn player
    | length (handFor player state) < 6 -> TakeCard (bestRank player (scoreDiffABRank state lookahead))
    | otherwise -> TakeCard (bestRank player (potentialTrimmedRank state lookahead))
  _ -> error "potentialTrimmedMinimaxPick: called on finished game"


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
  initialGame [(Nigiri 1),(Nigiri 1),(Nigiri 2),(Nigiri 3),(Wasabi Nothing),(Wasabi Nothing)
  ,Dumplings,Dumplings,Eel,Eel,Tofu,Tofu,Tofu,Sashimi,Sashimi] [(Nigiri 1),(Nigiri 2),(Nigiri 2),(Nigiri 3),
  (Wasabi Nothing),(Wasabi Nothing),Dumplings,Dumplings,Eel,Eel,Eel,Tofu,Tofu,Sashimi,Sashimi]

cards1 :: [Card]
cards1 =
  [(Nigiri 1),(Nigiri 1),(Nigiri 2),(Nigiri 3),(Wasabi Nothing),(Wasabi Nothing)
  ,Dumplings,Dumplings,Eel,Eel,Tofu,Tofu,Tofu,Sashimi,Sashimi]

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

midState4 :: GameState
midState4 =
  GameState (Turn Player1) [Eel,Eel] [] [Sashimi,Sashimi] []

midState5 :: GameState
midState5 =
  GameState (Turn Player1) [(Nigiri 1),(Nigiri 3),(Wasabi Nothing),Sashimi,Sashimi,Tofu]
  [(Nigiri 1),Sashimi] [(Nigiri 2),(Nigiri 2),(Wasabi Nothing),Tofu,Tofu,Eel] [Eel,Eel]

midState6 :: GameState
midState6 =
  GameState (Turn Player1) [Sashimi,(Nigiri 1),(Nigiri 1),Dumplings,Dumplings,
  Eel,Eel,Eel,Eel,Eel,Tofu,Tofu,Tofu] [(Nigiri 2),(Nigiri 3)]
  [(Nigiri 1),(Nigiri 2),(Nigiri 2),(Nigiri 2),Eel,Eel,Eel,Eel,Eel,Tofu,Tofu,
  Tofu,Tofu] [(Nigiri 2),(Nigiri 3)]

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
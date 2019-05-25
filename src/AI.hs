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

data GSTree a = GSTree a [GSTree a]

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

largeGameState :: GameState
largeGameState = initialGame [(Nigiri 1),(Nigiri 1),(Wasabi Nothing),Dumplings,Eel,Tofu,Sashimi] [(Nigiri 2),(Nigiri 2),(Wasabi Nothing),Dumplings,Eel,Tofu,Sashimi]

mediumGameState :: GameState
mediumGameState = initialGame [(Nigiri 1),Dumplings,Tofu] [(Nigiri 2),Eel,Sashimi]

smallGameState :: GameState
smallGameState = initialGame [(Nigiri 1),Dumplings] [(Nigiri 2),Eel]

tinyGameState :: GameState
tinyGameState = initialGame [Dumplings] [Eel]

-- | The table of all AIs you have implemented. We will mark the AI
-- called "default" as your submission, but you may include other AIs
-- for testing.
ais :: [(String, AIFunc)]
ais = [("firstLegal", firstCard),("greedy", greedyCard)]

-- Equivalently: firstLegal :: GameState -> Int -> Move
-- firstLegal simply takes the first card it sees
firstCard :: AIFunc
firstCard state _ = case gameStatus state of
  Turn player -> TakeCard (head (handFor player state))
  _ -> error "firstCard: called on finished game"

greedyCard :: AIFunc
greedyCard state _ = case gameStatus state of
  Turn player -> TakeCard (greedyGreatest (greedyHeuristic (handFor player state) (cardsFor player state) []))
  _ -> error "greedyCard: called on finished game"

greedyHeuristic  :: [Card] -> [Card] -> [(Card,Int)] -> [(Card,Int)]
greedyHeuristic hand chosenCards carry = case hand of
  [] -> carry
  x:xs -> greedyHeuristic xs chosenCards ((x,(scoreCards (x : chosenCards))) : carry)

greedyGreatest :: [(Card,Int)] -> Card
greedyGreatest handOptions = case handOptions of
  [] -> error "greedyGreatest: hand is empty"
  x:xs -> greedyGreatestHelper xs x

  where
    greedyGreatestHelper :: [(Card,Int)] -> (Card,Int) -> Card
    greedyGreatestHelper options (currCard,currScore) = case options of
      [] -> currCard
      (card,score):xs
        | score > currScore -> greedyGreatestHelper xs (card,score)
        | otherwise -> greedyGreatestHelper xs (currCard,currScore)

gsMoves :: GameState -> [GameState]
gsMoves gameState@(GameState s p1h _ p2h _) = case s of
  Turn Player1 -> gsMovesHelper gameState p1h []
  Turn Player2 -> gsMovesHelper gameState p2h []
  _ -> [gameState]

  where
    gsMovesHelper :: GameState -> [Card] -> [GameState] -> [GameState]
    gsMovesHelper gs remHand carry = case remHand of
      [] -> carry
      x:xs -> gsMovesHelper gs xs (playMove (TakeCard x) gs : carry)

buildGSTree :: GameState -> GSTree GameState
buildGSTree gs = case gameStatus gs of
  Finished -> GSTree gs []
  _ -> GSTree gs (map buildGSTree (gsMoves gs))
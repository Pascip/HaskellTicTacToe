module TicTac where

import Data.List
import Data.Maybe
import Data.Char
-- First attempt

n :: Int
n = 3

data Player = X | O deriving (Show, Read, Eq)
type Marking = Maybe Player
type Position = (Int, Int)
type Grid = [[Marking]]

emptyLine :: [Marking]
emptyLine = [Nothing | _ <- [1..n]]

emptyGrid :: Grid
emptyGrid = [emptyLine | _ <- [1..n]]

data Game = Game {
    board :: Grid,
    curTurn :: Player
} deriving Show

runningGame :: Game
runningGame = Game {
  board = emptyGrid,
  curTurn = X
}

makeMove :: Game -> Position -> Game
makeMove (Game grid player) (row, column) = Game {
  board = gridAfter,
  curTurn = nextPlayer
} where
  nextPlayer = if player == X then O else X
  gridAfter = changeGrid grid (row, column) player
  -- gridAfter = if isNothing $ grid !! row !! column
    -- then changeGrid grid (row, column) player
    -- else error "YO that's illegal"

changeGrid :: Grid -> Position -> Player -> Grid
changeGrid [] _ _ =  []
changeGrid (xs:xss) (row, column) player
  | column == 0 = (changeLine xs row player):xss
  | otherwise = xs : changeGrid xss (row, column-1) player

changeLine :: [Marking] -> Int -> Player -> [Marking]
changeLine [] _ _ = []
changeLine (x:xs) row player
  | row == 0 = Just player:xs
  | otherwise = x:changeLine xs (row-1) player

winSeqs :: Grid -> [[Marking]]
winSeqs grid = horizontal ++ vertical ++ [fDiag, bDiag]
  where horizontal = grid
        vertical = transpose grid
        fDiag = zipWith (!!) (reverse grid) [0..]
        bDiag = zipWith (!!) grid [0..]

checkForWin :: Game -> Maybe Player
checkForWin (Game grid _)
  | isWin' X = Just X
  | isWin' O = Just O
  | otherwise = Nothing
  where
    isWin':: Player -> Bool
    isWin' player = any (all (== Just player)) $ winSeqs grid


gameTesta = makeMove runningGame (0,1)
gameTestb = makeMove gameTesta (1,0)
gameTestc = makeMove gameTestb (0,0)
gameTestd = makeMove gameTestc (1,2)
gameTeste = makeMove gameTestd (0,2)

-- implement logic
--startGame :: IO ()
--startGame = do
--  putStrLn "Player X please choose your row and Column"
--  x <- getChar
--  y <- getChar
--  print (makeMove runningGame (ord x, ord y))
--  putStrLn " ok ok "
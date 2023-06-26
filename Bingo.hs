module Bingo () where

import Data.List (transpose)
import State (State (..), get, put)

grid :: [[Int]]
grid =
  [ [48, 69, 68, 49, 13],
    [25, 14, 30, 74, 89],
    [16, 38, 19, 24, 29],
    [56, 97, 50, 65, 79],
    [57, 52, 05, 27, 76]
  ]

yolo = runState (playRound' [48, 69, 68, 49, 13, 25]) (initBoard grid)

playRound :: Int -> State [[(Int, Bool)]] Int
playRound num = do
  board <- get
  put $ markBoard board num
  return num

playRound' :: [Int] -> State [[(Int, Bool)]] ()
playRound' [] = return ()
playRound' (x : xs) = do
  board <- get
  let newBoard = markBoard board x
  put newBoard
  if isBoardBingo newBoard then return () else playRound' xs

markBoard :: [[(Int, Bool)]] -> Int -> [[(Int, Bool)]]
markBoard [] _ = []
markBoard (row : rest) number = map (markCell number) row : markBoard rest number
  where
    markCell num elem@(a, _)
      | num == a = (a, True)
      | otherwise = elem

initBoard :: [[Int]] -> [[(Int, Bool)]]
initBoard = map (map (,False))

isBoardBingo :: [[(Int, Bool)]] -> Bool
isBoardBingo board =
  any areNeighborsCompleted board
    || any areNeighborsCompleted (transpose board)
  where
    areNeighborsCompleted = all snd

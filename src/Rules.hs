module Rules where

import Type
import Neighbors

{- 
  ** Understanding Rules **
  Based on the number of alive neighbors we can determine which cell will survive, die or reborn
  - `B` followed by number (eg. B23) => denotes that if a dead cell has either 2 or 3 alive neighbors it will be reborn (become alive)
  - `S` followed by number (eg, S56) => denotes that if an alive cell has either 5 or 6 alive neighbors it will be continue to live
  - If cell does not satisfies any of the above cases/conditions will die
-}

genGameOfLife :: (Int, Int) -> [(Int, Row)] -> Int -> [Cell] -> Cell -> Cell
genGameOfLife dimension allRows cellsInCurrentRow rowNumber (Cell isAlive _ colNumber _) = do
  let aliveCount = getAliveCountOn4Neighbors dimension allRows cellsInCurrentRow rowNumber colNumber
  case (aliveCount, isAlive) of
    (2, True) -> Cell True isAlive colNumber aliveCount
    (3, True) -> Cell True isAlive colNumber aliveCount
    (4, True) -> Cell False isAlive colNumber aliveCount
    (_, _) -> Cell False isAlive colNumber aliveCount

genDayAndNight :: (Int, Int) -> [(Int, Row)] -> Int -> [Cell] -> Cell -> Cell
genDayAndNight dimension allRows cellsInCurrentRow rowNumber (Cell isAlive _ colNumber _) = do
  let aliveCount = getAliveCountOn8Neighbors dimension allRows cellsInCurrentRow rowNumber colNumber
  case (aliveCount, isAlive) of
    (3, _) -> Cell True isAlive colNumber aliveCount
    (4, True) -> Cell True isAlive colNumber aliveCount
    (6, _) -> Cell True isAlive colNumber aliveCount
    (7, _) -> Cell True isAlive colNumber aliveCount
    (8, _) -> Cell True isAlive colNumber aliveCount
    (_, _) -> Cell False isAlive colNumber aliveCount

genB257S27 :: (Int, Int) -> [(Int, Row)] -> Int -> [Cell] -> Cell -> Cell
genB257S27 dimension allRows cellsInCurrentRow rowNumber (Cell isAlive _ colNumber _) = do
  let aliveCount = getAliveCountOn8Neighbors dimension allRows cellsInCurrentRow rowNumber colNumber
  case (aliveCount, isAlive) of
    (2, _) -> Cell True isAlive colNumber aliveCount
    (5, False) -> Cell True isAlive colNumber aliveCount
    (7, _) -> Cell True isAlive colNumber aliveCount
    (_, _) -> Cell False isAlive colNumber aliveCount

genB3S23 :: (Int, Int) -> [(Int, Row)] -> Int -> [Cell] -> Cell -> Cell
genB3S23 dimension allRows cellsInCurrentRow rowNumber (Cell isAlive _ colNumber _) = do
  let aliveCount = getAliveCountOn8Neighbors dimension allRows cellsInCurrentRow rowNumber colNumber
  case (aliveCount, isAlive) of
    (2, True) -> Cell True isAlive colNumber aliveCount
    (3, _) -> Cell True isAlive colNumber aliveCount
    (_, _) -> Cell False isAlive colNumber aliveCount

genB367S2346 :: (Int, Int) -> [(Int, Row)] -> Int -> [Cell] -> Cell -> Cell
genB367S2346 dimension allRows cellsInCurrentRow rowNumber (Cell isAlive _ colNumber _) = do
  let aliveCount = getAliveCountOn8Neighbors dimension allRows cellsInCurrentRow rowNumber colNumber
  case (aliveCount, isAlive) of
    (2, True) -> Cell True isAlive colNumber aliveCount
    (3, _) -> Cell True isAlive colNumber aliveCount
    (4, True) -> Cell True isAlive colNumber aliveCount
    (6, _) -> Cell True isAlive colNumber aliveCount
    (7, False) -> Cell True isAlive colNumber aliveCount
    (_, _) -> Cell False isAlive colNumber aliveCount

genB35S236 :: (Int, Int) -> [(Int, Row)] -> Int -> [Cell] -> Cell -> Cell
genB35S236 dimension allRows cellsInCurrentRow rowNumber (Cell isAlive _ colNumber _) = do
  let aliveCount = getAliveCountOn8Neighbors dimension allRows cellsInCurrentRow rowNumber colNumber
  case (aliveCount, isAlive) of
    (2, True) -> Cell True isAlive colNumber aliveCount
    (3, _) -> Cell True isAlive colNumber aliveCount
    (5, False) -> Cell True isAlive colNumber aliveCount
    (6, True) -> Cell True isAlive colNumber aliveCount
    (_, _) -> Cell False isAlive colNumber aliveCount

genB345S3 :: (Int, Int) -> [(Int, Row)] -> Int -> [Cell] -> Cell -> Cell
genB345S3 dimension allRows cellsInCurrentRow rowNumber (Cell isAlive _ colNumber _) = do
  let aliveCount = getAliveCountOn8Neighbors dimension allRows cellsInCurrentRow rowNumber colNumber
  case (aliveCount, isAlive) of
    (3, _) -> Cell True isAlive colNumber aliveCount
    (4, False) -> Cell True isAlive colNumber aliveCount
    (5, False) -> Cell True isAlive colNumber aliveCount
    (_, _) -> Cell False isAlive colNumber aliveCount

genB3S3 :: (Int, Int) -> [(Int, Row)] -> Int -> [Cell] -> Cell -> Cell
genB3S3 dimension allRows cellsInCurrentRow rowNumber (Cell isAlive _ colNumber _) = do
  let aliveCount = getAliveCountOn8Neighbors dimension allRows cellsInCurrentRow rowNumber colNumber
  case (aliveCount, isAlive) of
    (3, _) -> Cell True isAlive colNumber aliveCount
    (_, _) -> Cell False isAlive colNumber aliveCount

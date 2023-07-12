module Neighbors where

import Type

-- For counting alive neighbors
getAliveCountOn4Neighbors :: (Int, Int) -> [(Int, Row)] -> Int -> [Cell] -> Int -> Int
getAliveCountOn4Neighbors (maxRow, maxCol) allRows cellsInCurrentRow rowNumber colNumber =
    checkUpper allRows cellsInCurrentRow colNumber
    + checkLower allRows maxRow cellsInCurrentRow colNumber
    + checkLeft allRows cellsInCurrentRow colNumber
    + checkRight allRows maxCol cellsInCurrentRow colNumber

getAliveCountOn8Neighbors :: (Int, Int) -> [(Int, Row)] -> Int -> [Cell] -> Int -> Int
getAliveCountOn8Neighbors (maxRow, maxCol) allRows cellsInCurrentRow rowNumber colNumber =
    checkUpper allRows cellsInCurrentRow colNumber
    + checkLower allRows maxRow cellsInCurrentRow colNumber
    + checkLeft allRows cellsInCurrentRow colNumber
    + checkRight allRows maxCol cellsInCurrentRow colNumber
    + checkUpperLeft allRows cellsInCurrentRow colNumber
    + checkUpperRight allRows maxCol cellsInCurrentRow colNumber
    + checkLowerLeft allRows maxRow cellsInCurrentRow colNumber
    + checkLowerRight allRows maxRow maxCol cellsInCurrentRow colNumber

checkUpper :: [(Int, Row)] -> Int -> Int -> Int
checkUpper rows currRow pos = do
  if currRow > 0
    then do
      let (Row cells) = snd $ rows !! (currRow - 1)
      checkHeartbeat $ cells !! pos
    else 0

checkLower :: [(Int, Row)] -> Int -> Int -> Int -> Int
checkLower rows bound currRow pos = do
  if currRow + 1 < bound
    then do
      let (Row cells) = snd $ rows !! (currRow + 1)
      checkHeartbeat $ cells !! pos
    else 0

checkLeft :: [(Int, Row)] -> Int -> Int -> Int
checkLeft rows currRow pos = do
  if pos > 0
    then do
      let (Row cells) = snd $ rows !! currRow
      checkHeartbeat $ cells !! (pos - 1)
    else 0

checkRight :: [(Int, Row)] -> Int -> Int -> Int -> Int
checkRight rows bound currRow pos = do
  if pos + 1 < bound
    then do
      let (Row cells) = snd $ rows !! currRow
      checkHeartbeat $ cells !! (pos + 1)
    else 0

checkUpperLeft :: [(Int, Row)] -> Int -> Int -> Int
checkUpperLeft rows currRow pos = do
  if pos > 0 && currRow > 0
    then do
      let (Row cells) = snd $ rows !! (currRow - 1)
      checkHeartbeat $ cells !! (pos - 1)
    else 0

checkUpperRight :: [(Int, Row)] -> Int -> Int -> Int -> Int
checkUpperRight rows bound currRow pos = do
  if pos + 1 < bound && currRow > 0
    then do
      let (Row cells) = snd $ rows !! (currRow - 1)
      checkHeartbeat $ cells !! (pos + 1)
    else 0

checkLowerLeft :: [(Int, Row)] -> Int -> Int -> Int -> Int
checkLowerLeft rows bound currRow pos = do
  if pos > 0 && currRow + 1 < bound
    then do
      let (Row cells) = snd $ rows !! (currRow + 1)
      checkHeartbeat $ cells !! (pos - 1)
    else 0

checkLowerRight :: [(Int, Row)] -> Int -> Int -> Int -> Int -> Int
checkLowerRight rows rowBound colBound currRow pos = do
  if pos + 1 < colBound && currRow + 1 < rowBound
    then do
      let (Row cells) = snd $ rows !! (currRow + 1)
      checkHeartbeat $ cells !! (pos + 1)
    else 0

checkHeartbeat :: Cell -> Int
checkHeartbeat (Cell _ isAlivePrev _ _) =
  if isAlivePrev
    then 1
    else 0
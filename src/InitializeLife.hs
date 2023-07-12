module InitializeLife where 

import Data.List.Index
import Type
import qualified StartingPatterns as SP

makeBool :: Int -> (Int, Int) -> IO [(Int, Bool)]
-- makeBool _ (_,maxCol) = SP.randomPattern maxCol

-- makeBool _ (_,maxCol) = SP.evenColumn maxCol

-- makeBool rowNumber (_,maxCol) = SP.checker rowNumber maxCol

makeBool = SP.centerSquare

-- makeBool = SP.invertedCenterSquare

-- makeBool = SP.centerTriangle

-- makeBool = SP.invertedCenterTriangle

-- makeBool = SP.centerT

-- makeBool = SP.invertedCenterT

-- makeBool = SP.somePattern


genCell :: (Int, Bool) -> Cell
genCell (index, life) = Cell {
      isAlive = life
    , isAlivePrev = life
    , pos = index
    , neighborsAlive = 0
    }

genRow :: Int -> (Int, Int) -> IO Row
genRow rowNumber dimensions = do
  livingCells <- makeBool rowNumber dimensions
  return $ Row $ map genCell livingCells

genBoard :: Int -> (Int, Int) -> [Row] -> IO [Row]
genBoard 0 _ board = return board
genBoard r dimensions rows = do
  rowNumber <- genRow r dimensions
  genBoard (r - 1) dimensions (rowNumber : rows)

initLife :: Int -> Int -> IO Board
initLife rows cols = do
  board <- genBoard rows (rows, cols) []
  return $ Board $ indexed board
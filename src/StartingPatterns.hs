{-# LANGUAGE ScopedTypeVariables #-}

module StartingPatterns where 

import Data.List.Index
import System.Random (randomIO)


-- randomPattern :: Int -> IO [(Int, Bool)] -- TODO fix replicateM error
-- randomPattern maxCol = indexed <$> replicateM maxCol randomIO

evenColumn :: Int -> IO [(Int, Bool)]
evenColumn maxCol = return $ indexed $ map even [0..(maxCol-1)]

checker :: Int -> Int -> IO [(Int, Bool)]
checker rowNumber maxCol = return $ indexed $
  map (\x -> (mod x 10 == 3 || mod rowNumber 10 == 3)) [0 .. (maxCol - 1)]

centerSquare :: Int -> (Int, Int) -> IO [(Int, Bool)]
centerSquare rowNumber (maxRow, maxCol) = 
  let boxCenterY = div maxRow 2
      boxCenterX = div maxCol 2
  in
    return $ indexed $
      map (genCenterSquare boxCenterY boxCenterX rowNumber) [0 .. (maxCol - 1)]

invertedCenterSquare :: Int -> (Int, Int) -> IO [(Int, Bool)]
invertedCenterSquare rowNumber (maxRow, maxCol) = 
  let boxCenterY = div maxRow 2
      boxCenterX = div maxCol 2
  in
    return $ indexed $
      map (not . genCenterSquare boxCenterY boxCenterX rowNumber) [0 .. (maxCol - 1)]

genCenterSquare :: Int -> Int -> Int -> Int -> Bool
genCenterSquare boxCenterY boxCenterX rowNumber colNumber = 
  let boxLenBy2 = 10
  in ((rowNumber < (boxCenterY + boxLenBy2)) 
    && (rowNumber > (boxCenterY - boxLenBy2)) 
    && (colNumber < (boxCenterX + boxLenBy2)) 
    && (colNumber > (boxCenterX - boxLenBy2)))

centerTriangle :: Int -> (Int, Int) -> IO [(Int, Bool)]
centerTriangle rowNumber (maxRow, maxCol) = 
  let boxCenterY = div maxRow 2
      boxCenterX = div maxCol 2
   in return $ indexed $ map (genCenterTriangle boxCenterY boxCenterX rowNumber) [0 .. (maxCol - 1)]

invertedCenterTriangle :: Int -> (Int, Int) -> IO [(Int, Bool)]
invertedCenterTriangle rowNumber (maxRow, maxCol) = 
  let boxCenterY = div maxRow 2
      boxCenterX = div maxCol 2
   in return $ indexed $ map (not . genCenterTriangle boxCenterY boxCenterX rowNumber) [0 .. (maxCol - 1)]

genCenterTriangle :: Int -> Int -> Int -> Int -> Bool
genCenterTriangle boxCenterY boxCenterX rowNumber colNumber =
  let boxLenBy2 = 10
   in if ((rowNumber < (boxCenterY + boxLenBy2))
        && (rowNumber > (boxCenterY - boxLenBy2))
        && (colNumber < (boxCenterX + boxLenBy2))
        && (colNumber > (boxCenterX - boxLenBy2))
        )
      then
        let rowDifference = abs rowNumber - boxCenterY
          in ((colNumber <= (boxCenterX + rowDifference))
              && (colNumber >= (boxCenterX - rowDifference)))
      else False
    
centerT :: Int -> (Int, Int) -> IO [(Int, Bool)]
centerT rowNumber (maxRow, maxCol) = 
  let boxCenterY = div maxRow 2
      boxCenterX = div maxCol 2
   in return $ indexed $ map (genCenterT boxCenterY boxCenterX rowNumber) [0 .. (maxCol - 1)]

invertedCenterT :: Int -> (Int, Int) -> IO [(Int, Bool)]
invertedCenterT rowNumber (maxRow, maxCol) = 
  let boxCenterY = div maxRow 2
      boxCenterX = div maxCol 2
   in return $ indexed $ map (not . genCenterT boxCenterY boxCenterX rowNumber) [0 .. (maxCol - 1)]

genCenterT :: Int -> Int -> Int -> Int -> Bool
genCenterT boxCenterY boxCenterX rowNumber colNumber =
  let boxLenBy2 = 10
    in (rowNumber < (boxCenterY + boxLenBy2))
        && (rowNumber > (boxCenterY - boxLenBy2))
        && (colNumber < (boxCenterX + boxLenBy2))
        && (colNumber > (boxCenterX - boxLenBy2))
        && ((colNumber == boxCenterX)
            || (rowNumber == boxCenterY - (boxLenBy2 - 1))
            )

somePattern :: Int -> (Int, Int) -> IO [(Int, Bool)]
somePattern rowNumber (maxRow, maxCol) = return $ indexed $
  map (gen3CellLine maxRow maxCol rowNumber) [0 .. maxCol]

gen3CellLine m2 n2 rowNumber col = do -- TODO -> not working properly
  let (rBlock :: Int) = div (rowNumber-1) 3
      (cBlock :: Int) = div (col-1) 3
    in 
      case odd rBlock && odd cBlock of
        True -> if mod col 3 == 2 then True else False
        False -> False
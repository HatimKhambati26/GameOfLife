{-# LANGUAGE InstanceSigs #-}

module Type where

-- Board --
newtype Board = Board [(Int, Row)]

instance Show Board where
  show :: Board -> String
  show (Board rows) = unlines $ map (\(_, row) -> show row) rows


-- Row --
newtype Row = Row [Cell]

instance Show Row where
  show :: Row -> String
  show (Row cells) = unwords $ map show cells


-- Cell --
data Cell = Cell {
      isAlive :: Bool
    , isAlivePrev :: Bool
    , pos :: Int
    , neighborsAlive :: Int
  }
  deriving (Eq)

instance Show Cell where
  show :: Cell -> String
  show (Cell isAlive isAlivePrev _ n) =
    -- case isAlivePrev of 
    --   True -> "T"
    --   False -> "F"
    if isAlive --  then "▦"
      then "▦"
      else " "
    -- show n
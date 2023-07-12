{-# LANGUAGE ScopedTypeVariables #-}
module GameOfLife where

import Control.Concurrent
import Control.Monad
import System.Console.ANSI (setCursorPosition, clearScreen, hideCursor)
import System.Random (randomIO)

import Type
import InitializeLife
import Rules

startLife :: Int -> Int -> Int -> IO ()
startLife rule allRows cols = do
  let (d3r :: Int) = div allRows 3
      (d3c :: Int) = div cols 3
      r3 = d3r * 3
      c3 = d3c * 3
  life <- initLife r3 c3
  iterLife rule (r3, c3) life

iterLife :: Int -> (Int, Int) -> Board -> IO ()
iterLife rule dimension board = do
  setCursorPosition 0 0
  print board
  threadDelay 100000 -- sleep For a million microseconds, or one second
  iterLife rule dimension $ nextBoard rule dimension board

nextBoard :: Int -> (Int, Int) -> Board -> Board
nextBoard rule dimension (Board allRows) =
  Board $
    map (nextRow rule dimension allRows) allRows

nextRow :: Int -> (Int, Int) -> [(Int, Row)] -> (Int, Row) -> (Int, Row)
nextRow rule dimension allRows (rowNumber, Row cells) =
  let generator = getGenerator rule
   in (rowNumber, Row $ map (generator dimension allRows rowNumber cells) cells)
 where
  getGenerator rule = case rule of
    1 -> genGameOfLife
    2 -> genDayAndNight
    3 -> genB257S27
    4 -> genB3S23
    5 -> genB367S2346
    6 -> genB35S236
    7 -> genB345S3
    _ -> genB3S3 -- Default
